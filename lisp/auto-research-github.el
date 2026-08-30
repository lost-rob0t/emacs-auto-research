;;; auto-research-github.el --- Optional GitHub backend for auto-research -*- lexical-binding: t; -*-

(require 'cl-lib)
(require 'json)
(require 'seq)
(require 'subr-x)
(require 'url-util)
(require 'auto-research-dashboard)
(require 'auto-research-metadata)
(require 'auto-research-plugin)

(defcustom auto-research-github-owners nil
  "GitHub owners scanned for remote research.
Nil means the authenticated `gh' user plus every visible organization."
  :type '(repeat string)
  :group 'auto-research)

(defcustom auto-research-github-max-workers 6
  "Maximum concurrent GitHub tree and blob fetches."
  :type 'integer
  :group 'auto-research)

(defcustom auto-research-github-show-legacy-default nil
  "Whether remote legacy review-ready research is included."
  :type 'boolean
  :group 'auto-research)

(defcustom auto-research-github-auto-merge-decisions t
  "Whether approval-only pull requests are merged immediately."
  :type 'boolean
  :group 'auto-research)

(defcustom auto-research-github-merge-method "rebase"
  "Merge method used for approval-only pull requests."
  :type '(choice (const "rebase") (const "squash") (const "merge"))
  :group 'auto-research)

(defconst auto-research-github--auxiliary-files
  '("index.org" "sources.org" "search-log.org"))
(defconst auto-research-github--legacy-reviewable-lifecycles
  '("REVIEW" "RESEARCHED" "VERIFIED"))
(defconst auto-research-github--approval-pr-marker
  "<!-- auto-research-approval:v1 -->")

(cl-defstruct (auto-research-github--scan
               (:constructor auto-research-github--scan-create))
  emit done processes queue active searches-left errors seen discovering finished)

(defvar auto-research-github--active-scan nil)

(defun auto-research-github--json (text)
  "Parse GitHub JSON TEXT into alists/lists."
  (json-parse-string text :object-type 'alist :array-type 'list
                     :null-object nil :false-object nil))

(defun auto-research-github--get (key object)
  "Read string KEY from JSON alist OBJECT."
  (alist-get key object nil nil #'string=))

(defun auto-research-github--cancel-scan (scan)
  "Cancel asynchronous processes belonging to SCAN."
  (when scan
    (setf (auto-research-github--scan-finished scan) t)
    (dolist (process (auto-research-github--scan-processes scan))
      (when (process-live-p process)
        (process-put process 'auto-research-github-cancelled t)
        (delete-process process)))
    (setf (auto-research-github--scan-processes scan) nil)))

(cl-defun auto-research-github--gh (callback args &key input scan)
  "Run `gh' asynchronously and invoke CALLBACK with (OK OUTPUT).
ARGS are passed directly to `gh'.  INPUT, when non-nil, is written to stdin.
When SCAN is non-nil, track the process so a newer refresh can cancel it."
  (if (not (executable-find "gh"))
      (funcall callback nil "GitHub CLI `gh' is required")
    (let ((buffer (generate-new-buffer " *auto-research-gh*"))
          process)
      (condition-case error-data
          (setq process
                (make-process
                 :name (make-temp-name "auto-research-gh-")
                 :buffer buffer
                 :command (cons "gh" args)
                 :connection-type 'pipe
                 :coding 'utf-8-unix
                 :noquery t
                 :sentinel
                 (lambda (proc _event)
                   (when (memq (process-status proc) '(exit signal))
                     (let ((ok (and (eq (process-status proc) 'exit)
                                    (zerop (process-exit-status proc))))
                           (output (if (buffer-live-p buffer)
                                       (with-current-buffer buffer (buffer-string))
                                     "")))
                       (when scan
                         (setf (auto-research-github--scan-processes scan)
                               (delq proc (auto-research-github--scan-processes scan))))
                       (when (buffer-live-p buffer)
                         (kill-buffer buffer))
                       (unless (process-get proc 'auto-research-github-cancelled)
                         (funcall callback ok output)))))))
        (error
         (when (buffer-live-p buffer)
           (kill-buffer buffer))
         (funcall callback nil (error-message-string error-data))))
      (when (and process scan)
        (push process (auto-research-github--scan-processes scan)))
      (when (and process input)
        (condition-case error-data
            (progn
              (process-send-string process input)
              (process-send-eof process))
          (error
           (process-put process 'auto-research-github-cancelled t)
           (when (process-live-p process)
             (delete-process process))
           (funcall callback nil (error-message-string error-data)))))
      process)))

(defun auto-research-github--gh-json (callback args &optional input scan)
  "Run `gh' and parse JSON before invoking CALLBACK."
  (auto-research-github--gh
   (lambda (ok output)
     (if (not ok)
         (funcall callback nil (string-trim output))
       (condition-case error-data
           (funcall callback t (auto-research-github--json output))
         (error
          (funcall callback nil (error-message-string error-data))))))
   args :input input :scan scan))

(defun auto-research-github--candidate-path-p (path)
  "Return non-nil when PATH can be a research Org document."
  (and (stringp path)
       (string-suffix-p ".org" path t)
       (not (string-prefix-p "../" path))
       (or (string-prefix-p "research/" path)
           (string-match-p "/research/" path))
       (not (member (downcase (file-name-nondirectory path))
                    auto-research-github--auxiliary-files))))

(defun auto-research-github--legacy-reviewable-p (lifecycle)
  "Return non-nil when LIFECYCLE was reviewable before approval metadata."
  (member (upcase (string-trim (or lifecycle "")))
          auto-research-github--legacy-reviewable-lifecycles))

(defun auto-research-github--remote-item (repo branch path blob content)
  "Build a remote dashboard item from GitHub document CONTENT."
  (let* ((title (auto-research-metadata-title content (file-name-base path)))
         (lifecycle (auto-research-metadata-lifecycle content))
         (approval (auto-research-metadata-approval-state content)))
    (when (and (auto-research-github--candidate-path-p path)
               (or (string= approval "PENDING")
                   (and auto-research-github-show-legacy-default
                        (string= approval "LEGACY")
                        (auto-research-github--legacy-reviewable-p lifecycle))))
      (auto-research-dashboard-make-item
       :id (format "github::%s::%s" repo path)
       :display-project repo
       :path path
       :title title
       :lifecycle lifecycle
       :approval approval
       :open-function #'auto-research-github-open-item
       :decision-function #'auto-research-github-decide-item
       :data (list :repo repo :branch branch :path path :blob blob :content content)))))

(defun auto-research-github--scan-error (scan text)
  "Record TEXT as a SCAN error."
  (push text (auto-research-github--scan-errors scan)))

(defun auto-research-github--scan-finish (scan)
  "Finish SCAN once discovery and queued jobs are exhausted."
  (when (and (not (auto-research-github--scan-finished scan))
             (not (auto-research-github--scan-discovering scan))
             (zerop (auto-research-github--scan-searches-left scan))
             (zerop (auto-research-github--scan-active scan))
             (null (auto-research-github--scan-queue scan)))
    (setf (auto-research-github--scan-finished scan) t)
    (funcall (auto-research-github--scan-done scan)
             (when-let ((errors (nreverse (auto-research-github--scan-errors scan))))
               (string-join errors "\n")))))

(defun auto-research-github--job-done (scan &optional item error-text)
  "Complete one SCAN job, optionally emitting ITEM or ERROR-TEXT."
  (when item
    (funcall (auto-research-github--scan-emit scan) (list item)))
  (when error-text
    (auto-research-github--scan-error scan error-text))
  (cl-decf (auto-research-github--scan-active scan))
  (auto-research-github--pump scan)
  (auto-research-github--scan-finish scan))

(defun auto-research-github--blob-job (scan repo branch path blob)
  "Fetch one research blob for SCAN."
  (auto-research-github--gh-json
   (lambda (ok result)
     (if (not ok)
         (auto-research-github--job-done
          scan nil (format "%s:%s: %s" repo path result))
       (condition-case error-data
           (let ((encoding (auto-research-github--get "encoding" result))
                 (content (auto-research-github--get "content" result)))
             (unless (and (string= encoding "base64") (stringp content))
               (error "unsupported blob encoding"))
             (auto-research-github--job-done
              scan
              (auto-research-github--remote-item
               repo branch path blob
               (decode-coding-string (base64-decode-string content) 'utf-8))))
         (error
          (auto-research-github--job-done
           scan nil
           (format "%s:%s: %s" repo path (error-message-string error-data)))))))
   (list "api" (format "repos/%s/git/blobs/%s" repo blob))
   nil scan))

(defun auto-research-github--tree-job (scan repo branch)
  "Enumerate remote research candidates in REPO at BRANCH."
  (auto-research-github--gh-json
   (lambda (ok result)
     (cond
      ((not ok)
       (auto-research-github--job-done
        scan nil (format "%s@%s tree: %s" repo branch result)))
      ((auto-research-github--get "truncated" result)
       (auto-research-github--job-done
        scan nil
        (format "%s@%s: recursive Git tree was truncated; scan is incomplete"
                repo branch)))
      (t
       (dolist (entry (auto-research-github--get "tree" result))
         (let ((path (auto-research-github--get "path" entry))
               (type (auto-research-github--get "type" entry))
               (blob (auto-research-github--get "sha" entry)))
           (when (and (string= type "blob") blob
                      (auto-research-github--candidate-path-p path))
             (let ((id (concat repo ":" path)))
               (unless (gethash id (auto-research-github--scan-seen scan))
                 (puthash id t (auto-research-github--scan-seen scan))
                 (setf (auto-research-github--scan-queue scan)
                       (nconc (auto-research-github--scan-queue scan)
                              (list (list 'blob repo branch path blob)))))))))
       (auto-research-github--job-done scan))))
   (list "api" "--method" "GET"
         (format "repos/%s/git/trees/%s" repo (url-hexify-string branch))
         "-f" "recursive=1")
   nil scan))

(defun auto-research-github--start-job (scan job)
  "Start one queued SCAN JOB."
  (pcase job
    (`(tree ,repo ,branch)
     (auto-research-github--tree-job scan repo branch))
    (`(blob ,repo ,branch ,path ,blob)
     (auto-research-github--blob-job scan repo branch path blob))
    (_
     (auto-research-github--job-done scan nil (format "Invalid GitHub job: %S" job)))))

(defun auto-research-github--pump (scan)
  "Start queued SCAN jobs up to the configured worker bound."
  (while (and (not (auto-research-github--scan-finished scan))
              (< (auto-research-github--scan-active scan)
                 (max 1 auto-research-github-max-workers))
              (auto-research-github--scan-queue scan))
    (cl-incf (auto-research-github--scan-active scan))
    (auto-research-github--start-job
     scan (pop (auto-research-github--scan-queue scan)))))

(defun auto-research-github--owner-repos-args (owner login)
  "Return `gh' arguments used to list repositories for OWNER."
  (if (string= owner login)
      (list "api" "--method" "GET" "--paginate" "user/repos"
            "-f" "affiliation=owner" "-f" "per_page=100"
            "--jq" ".[] | [.full_name, (.default_branch // \"\")] | @tsv")
    (list "api" "--method" "GET" "--paginate" (format "orgs/%s/repos" owner)
          "-f" "type=all" "-f" "per_page=100"
          "--jq" ".[] | [.full_name, (.default_branch // \"\")] | @tsv")))

(defun auto-research-github--list-owner-repos (scan owner login)
  "List OWNER repositories and enqueue tree scans."
  (auto-research-github--gh
   (lambda (ok output)
     (if (not ok)
         (auto-research-github--scan-error
          scan (format "%s repositories: %s" owner (string-trim output)))
       (dolist (line (split-string output "\n" t))
         (pcase-let ((`(,repo ,branch) (split-string line "\t" nil)))
           (if (or (not repo) (string-empty-p (or branch "")))
               (auto-research-github--scan-error
                scan (format "%s: repository has no default branch" (or repo owner)))
             (setf (auto-research-github--scan-queue scan)
                   (nconc (auto-research-github--scan-queue scan)
                          (list (list 'tree repo branch))))))))
     (cl-decf (auto-research-github--scan-searches-left scan))
     (auto-research-github--pump scan)
     (auto-research-github--scan-finish scan))
   (auto-research-github--owner-repos-args owner login)
   :scan scan))

(defun auto-research-github--start-owner-searches (scan login owners)
  "Start repository listing for OWNERS."
  (setf (auto-research-github--scan-discovering scan) nil
        (auto-research-github--scan-searches-left scan) (length owners))
  (dolist (owner owners)
    (auto-research-github--list-owner-repos scan owner login))
  (auto-research-github--scan-finish scan))

(defun auto-research-github--discover (scan)
  "Discover authenticated GitHub ownership scope for SCAN."
  (auto-research-github--gh
   (lambda (ok output)
     (if (not ok)
         (progn
           (auto-research-github--scan-error scan (string-trim output))
           (setf (auto-research-github--scan-discovering scan) nil)
           (auto-research-github--scan-finish scan))
       (let ((login (string-trim output)))
         (if auto-research-github-owners
             (auto-research-github--start-owner-searches
              scan login (delete-dups (copy-sequence auto-research-github-owners)))
           (auto-research-github--gh
            (lambda (org-ok org-output)
              (unless org-ok
                (auto-research-github--scan-error scan (string-trim org-output)))
              (auto-research-github--start-owner-searches
               scan login
               (delete-dups
                (cons login (if org-ok (split-string org-output "\n" t) nil)))))
            (list "api" "--paginate" "user/orgs" "--jq" ".[].login")
            :scan scan))))
   (list "api" "user" "--jq" ".login")
   :scan scan)))

(defun auto-research-github-dashboard-provider (scope emit done)
  "Provide remote GitHub dashboard items for SCOPE.
EMIT receives lists of dashboard items as they arrive.  DONE receives an
optional error string.  Remote discovery participates only in the all-projects
scope so selecting a local project stays strictly local."
  (if (not (eq scope 'all))
      (funcall done nil)
    (auto-research-github--cancel-scan auto-research-github--active-scan)
    (let ((scan (auto-research-github--scan-create
                 :emit emit
                 :done done
                 :processes nil
                 :queue nil
                 :active 0
                 :searches-left 0
                 :errors nil
                 :seen (make-hash-table :test #'equal)
                 :discovering t
                 :finished nil)))
      (setq auto-research-github--active-scan scan)
      (auto-research-github--discover scan))))

(defun auto-research-github-open-item (item)
  "Open remote GitHub dashboard ITEM in a read-only Org buffer."
  (let* ((data (auto-research-item-data item))
         (repo (plist-get data :repo))
         (path (plist-get data :path))
         (buffer (get-buffer-create (format "*Research: %s:%s*" repo path))))
    (with-current-buffer buffer
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert (plist-get data :content))
        (org-mode)
        (read-only-mode 1)
        (setq-local header-line-format (format "%s — %s" repo path))))
    (pop-to-buffer buffer)))

(defun auto-research-github--encode-path (path)
  "Percent-encode each component of repository PATH."
  (mapconcat #'url-hexify-string (split-string path "/" t) "/"))

(defun auto-research-github--branch-head (repo branch callback)
  "Resolve REPO BRANCH head SHA and invoke CALLBACK."
  (auto-research-github--gh-json
   (lambda (ok result)
     (if ok
         (funcall callback t
                  (auto-research-github--get
                   "sha" (auto-research-github--get "commit" result)))
       (funcall callback nil result)))
   (list "api" (format "repos/%s/branches/%s" repo (url-hexify-string branch)))))

(defun auto-research-github--remote-file (repo path commit callback)
  "Fetch REPO PATH at COMMIT and invoke CALLBACK with (BLOB . CONTENT)."
  (auto-research-github--gh-json
   (lambda (ok result)
     (if (not ok)
         (funcall callback nil result)
       (let ((content (auto-research-github--get "content" result))
             (encoding (auto-research-github--get "encoding" result)))
         (if (not (and (string= encoding "base64") (stringp content)))
             (funcall callback nil "unsupported contents encoding")
           (funcall callback t
                    (cons (auto-research-github--get "sha" result)
                          (decode-coding-string
                           (base64-decode-string content) 'utf-8)))))))
   (list "api" "--method" "GET"
         (format "repos/%s/contents/%s" repo
                 (auto-research-github--encode-path path))
         "-f" (concat "ref=" commit))))

(defun auto-research-github--decision-branch-name (path)
  "Return a collision-resistant approval branch name for PATH."
  (let ((stem (replace-regexp-in-string
               "[^A-Za-z0-9._-]+" "-" (file-name-base path))))
    (format "research-approval/%s-%s-%04x"
            (downcase stem)
            (format-time-string "%Y%m%d%H%M%S")
            (random #x10000))))

(defun auto-research-github--decision-commit-message (path state)
  "Return approval-only commit message for PATH and STATE."
  (format "docs(research): %s %s [skip ci]"
          (downcase state) (file-name-base path)))

(defun auto-research-github--confirm (repo path updated state continue cancel)
  "Show UPDATED decision for REPO PATH, then call CONTINUE or CANCEL."
  (let ((buffer (get-buffer-create "*Research Approval Preview*")))
    (with-current-buffer buffer
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert (format "%s\n%s\n\n" repo path))
        (dolist (field auto-research-metadata--approval-fields)
          (insert (format "#+%s: %s\n" field
                          (auto-research-metadata-keyword updated field))))
        (insert (format "\nDelivery: approval-only PR, [skip ci]%s\n"
                        (if auto-research-github-auto-merge-decisions
                            (format ", immediate %s merge" auto-research-github-merge-method)
                          "")))
        (special-mode)))
    (display-buffer buffer)
    (if (yes-or-no-p (format "%s %s/%s? " state repo (file-name-nondirectory path)))
        (funcall continue)
      (funcall cancel))))

(defun auto-research-github--create-branch (repo commit branch callback)
  "Create BRANCH in REPO at COMMIT."
  (auto-research-github--gh-json
   callback
   (list "api" "--method" "POST" (format "repos/%s/git/refs" repo) "--input" "-")
   (json-encode `((ref . ,(concat "refs/heads/" branch)) (sha . ,commit)))))

(defun auto-research-github--write-branch-file (repo branch path blob updated state callback)
  "Write UPDATED research PATH to approval BRANCH in REPO."
  (auto-research-github--gh-json
   callback
   (list "api" "--method" "PUT"
         (format "repos/%s/contents/%s" repo (auto-research-github--encode-path path))
         "--input" "-")
   (json-encode
    `((message . ,(auto-research-github--decision-commit-message path state))
      (content . ,(base64-encode-string updated t))
      (sha . ,blob)
      (branch . ,branch)))))

(defun auto-research-github--create-pr (repo base branch path state actor evidence callback)
  "Open approval-only pull request in REPO and invoke CALLBACK."
  (auto-research-github--gh-json
   callback
   (list "api" "--method" "POST" (format "repos/%s/pulls" repo) "--input" "-")
   (json-encode
    `((title . ,(format "docs(research): %s %s" (downcase state) (file-name-base path)))
      (head . ,branch)
      (base . ,base)
      (body . ,(format
                "%s\n\nHuman research decision recorded by emacs-auto-research.\n\nDecision: `%s`\nActor: `%s`\nEvidence: `%s`\n\nApproval-only metadata change. CI intentionally skipped."
                auto-research-github--approval-pr-marker state actor evidence))))))

(defun auto-research-github--merge-pr (repo number commit callback)
  "Merge REPO pull request NUMBER at expected COMMIT."
  (auto-research-github--gh-json
   callback
   (list "api" "--method" "PUT" (format "repos/%s/pulls/%s/merge" repo number)
         "--input" "-")
   (json-encode `((merge_method . ,auto-research-github-merge-method)
                  (sha . ,commit)))))

(defun auto-research-github--deliver-decision
    (repo branch path blob commit updated state actor evidence done)
  "Create approval branch/PR after the final race checks have passed."
  (let ((approval-branch (auto-research-github--decision-branch-name path)))
    (auto-research-github--create-branch
     repo commit approval-branch
     (lambda (ok result)
       (if (not ok)
           (funcall done nil (format "Could not create approval branch: %s" result))
         (auto-research-github--write-branch-file
          repo approval-branch path blob updated state
          (lambda (write-ok write-result)
            (if (not write-ok)
                (funcall done nil (format "Could not write approval branch: %s" write-result))
              (let* ((commit-object (auto-research-github--get "commit" write-result))
                     (approval-commit (auto-research-github--get "sha" commit-object)))
                (auto-research-github--create-pr
                 repo branch approval-branch path state actor evidence
                 (lambda (pr-ok pr-result)
                   (if (not pr-ok)
                       (funcall done nil (format "Could not open approval PR: %s" pr-result))
                     (let ((number (auto-research-github--get "number" pr-result))
                           (url (auto-research-github--get "html_url" pr-result)))
                       (if (not auto-research-github-auto-merge-decisions)
                           (funcall done t (format "Approval PR #%s opened: %s" number url))
                         (auto-research-github--merge-pr
                          repo number approval-commit
                          (lambda (merge-ok merge-result)
                            (cond
                             ((not merge-ok)
                              (funcall done nil
                                       (format "Approval PR #%s could not merge: %s\n%s"
                                               number merge-result url)))
                             ((auto-research-github--get "merged" merge-result)
                              (funcall done t
                                       (format "Approval PR #%s merged" number)))
                             (t
                              (funcall done nil
                                       (format "Approval PR #%s was not merged: %s\n%s"
                                               number
                                               (or (auto-research-github--get "message" merge-result)
                                                   merge-result)
                                               url))))))))))))))))))))

(defun auto-research-github--final-recheck
    (repo branch path blob content commit updated state actor evidence done)
  "Recheck branch and file identity immediately before delivery."
  (auto-research-github--branch-head
   repo branch
   (lambda (ok current-head)
     (if (not ok)
         (funcall done nil current-head)
       (if (not (string= current-head commit))
           (funcall done nil "Branch changed during review; refresh first")
         (auto-research-github--remote-file
          repo path commit
          (lambda (file-ok remote)
            (if (not file-ok)
                (funcall done nil remote)
              (if (not (and (string= blob (car remote))
                            (string= content (cdr remote))))
                  (funcall done nil "Research changed during review; refresh first")
                (auto-research-github--deliver-decision
                 repo branch path blob commit updated state actor evidence done))))))))))

(defun auto-research-github-decide-item (item state actor evidence done)
  "Record STATE for remote dashboard ITEM through an approval-only GitHub PR.
DONE receives (OK MESSAGE)."
  (let* ((data (auto-research-item-data item))
         (repo (plist-get data :repo))
         (branch (plist-get data :branch))
         (path (plist-get data :path))
         (discovered-blob (plist-get data :blob))
         (discovered-content (plist-get data :content)))
    (auto-research-github--branch-head
     repo branch
     (lambda (ok commit)
       (if (not ok)
           (funcall done nil commit)
         (auto-research-github--remote-file
          repo path commit
          (lambda (file-ok remote)
            (if (not file-ok)
                (funcall done nil remote)
              (let ((blob (car remote))
                    (content (cdr remote)))
                (if (not (and (string= blob discovered-blob)
                              (string= content discovered-content)))
                    (funcall done nil "Research changed since refresh; refresh first")
                  (condition-case error-data
                      (let ((updated
                             (auto-research-metadata-decide
                              content state actor evidence commit blob)))
                        (auto-research-github--confirm
                         repo path updated state
                         (lambda ()
                           (auto-research-github--final-recheck
                            repo branch path blob content commit updated
                            state actor evidence done))
                         (lambda ()
                           (funcall done nil "Decision cancelled"))))
                    (error
                     (funcall done nil (error-message-string error-data))))))))))))))

(defun auto-research-github-enable ()
  "Enable GitHub remote discovery and approval delivery in the unified dashboard."
  (interactive)
  (auto-research-register-plugin
   (auto-research-plugin-create
    :id 'github
    :dashboard-provider #'auto-research-github-dashboard-provider))
  (when (derived-mode-p 'auto-research-dashboard-mode)
    (auto-research-dashboard-refresh)))

(defun auto-research-github-disable ()
  "Disable GitHub remote discovery in the unified dashboard."
  (interactive)
  (auto-research-github--cancel-scan auto-research-github--active-scan)
  (setq auto-research-github--active-scan nil)
  (auto-research-unregister-plugin 'github)
  (when (derived-mode-p 'auto-research-dashboard-mode)
    (auto-research-dashboard-refresh)))

(provide 'auto-research-github)
;;; auto-research-github.el ends here
