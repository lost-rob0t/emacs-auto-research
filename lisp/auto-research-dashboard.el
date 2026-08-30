;;; auto-research-dashboard.el --- Unified research dashboard -*- lexical-binding: t; -*-

(require 'cl-lib)
(require 'org)
(require 'seq)
(require 'subr-x)
(require 'tabulated-list)
(require 'auto-research-project)
(require 'auto-research-metadata)
(require 'auto-research-plugin)

(cl-defstruct (auto-research-item (:constructor auto-research-item--create))
  project file title lifecycle approval id display-project path
  open-function decision-function data busy)

(defvar-local auto-research-dashboard--items nil)
(defvar-local auto-research-dashboard--scope 'all)
(defvar-local auto-research-dashboard--search "")
(defvar-local auto-research-dashboard--source-generation 0)
(defvar-local auto-research-dashboard--pending-sources 0)
(defvar-local auto-research-dashboard--errors nil)

(cl-defun auto-research-dashboard-make-item
    (&key project file title lifecycle approval id display-project path
          open-function decision-function data)
  "Create a dashboard item for local or external research.

External items should supply a stable ID, DISPLAY-PROJECT, PATH, OPEN-FUNCTION
and, when human decisions are supported, DECISION-FUNCTION.  DECISION-FUNCTION
is called as (ITEM STATE ACTOR EVIDENCE DONE), where DONE accepts (OK MESSAGE)."
  (auto-research-item--create
   :project project
   :file file
   :title title
   :lifecycle lifecycle
   :approval approval
   :id id
   :display-project display-project
   :path path
   :open-function open-function
   :decision-function decision-function
   :data data
   :busy nil))

(defun auto-research-dashboard--item-id (item)
  "Return stable dashboard identity for ITEM."
  (or (auto-research-item-id item)
      (let ((project (auto-research-item-project item))
            (file (auto-research-item-file item)))
        (unless (and project file)
          (user-error "External dashboard item is missing a stable :id"))
        (format "%s::%s"
                (auto-research-project-id project)
                (file-truename file)))))

(defun auto-research-dashboard--item-project-name (item)
  "Return display project name for ITEM."
  (or (auto-research-item-display-project item)
      (when-let ((project (auto-research-item-project item)))
        (auto-research-project-name project))
      "External"))

(defun auto-research-dashboard--item-path (item)
  "Return display path for ITEM."
  (or (auto-research-item-path item)
      (let ((file (auto-research-item-file item))
            (project (auto-research-item-project item)))
        (cond
         ((and file project)
          (file-relative-name file (auto-research-project-root project)))
         (file file)
         (t "(external)")))))

(defun auto-research-dashboard--read-file (project file)
  "Read FILE into an `auto-research-item' for PROJECT."
  (with-temp-buffer
    (insert-file-contents file)
    (let ((content (buffer-string)))
      (auto-research-dashboard-make-item
       :project project
       :file file
       :title (auto-research-metadata-title content (file-name-base file))
       :lifecycle (auto-research-metadata-lifecycle content)
       :approval (auto-research-metadata-approval-state content)))))

(defun auto-research-dashboard--projects-in-scope ()
  "Return projects selected by the current dashboard scope."
  (if (eq auto-research-dashboard--scope 'all)
      (auto-research-project-list)
    (let ((project (auto-research-project-by-id auto-research-dashboard--scope)))
      (if project (list project) nil))))

(defun auto-research-dashboard--scan-local ()
  "Scan local research files for the current dashboard scope."
  (setq auto-research-dashboard--items
        (cl-loop for project in (auto-research-dashboard--projects-in-scope)
                 append
                 (cl-loop for file in (auto-research-project-research-files project)
                          collect (auto-research-dashboard--read-file project file)))))

(defun auto-research-dashboard--visible-p (item)
  "Return non-nil when ITEM matches the current search."
  (or (string-empty-p auto-research-dashboard--search)
      (let ((needle (downcase auto-research-dashboard--search))
            (haystack
             (downcase
              (string-join
               (list (auto-research-dashboard--item-project-name item)
                     (auto-research-item-title item)
                     (auto-research-item-lifecycle item)
                     (auto-research-item-approval item)
                     (auto-research-dashboard--item-path item))
               "\n"))))
        (string-match-p (regexp-quote needle) haystack))))

(defun auto-research-dashboard--row (item)
  "Return tabulated row for ITEM."
  (let ((approval (auto-research-item-approval item)))
    (list
     (auto-research-dashboard--item-id item)
     (vector
      (if (auto-research-item-busy item)
          (concat approval " …")
        approval)
      (auto-research-dashboard--item-project-name item)
      (auto-research-item-lifecycle item)
      (auto-research-item-title item)
      (auto-research-dashboard--item-path item)))))

(defun auto-research-dashboard--scope-label ()
  "Return human-readable scope label."
  (if (eq auto-research-dashboard--scope 'all)
      "ALL PROJECTS"
    (let ((project (auto-research-project-by-id auto-research-dashboard--scope)))
      (if project (auto-research-project-name project) "UNKNOWN"))))

(defun auto-research-dashboard--render ()
  "Render the dashboard."
  (let ((visible (seq-filter #'auto-research-dashboard--visible-p
                             auto-research-dashboard--items)))
    (setq tabulated-list-entries (mapcar #'auto-research-dashboard--row visible)
          header-line-format
          (format
           "Auto Research — %s — %d/%d%s%s  [RET open] [a approve] [R reject] [n new] [p project] [A all] [x plugin] [/ search] [g refresh] [e errors]"
           (auto-research-dashboard--scope-label)
           (length visible)
           (length auto-research-dashboard--items)
           (if (> auto-research-dashboard--pending-sources 0)
               (format " — remote:%d" auto-research-dashboard--pending-sources)
             "")
           (if auto-research-dashboard--errors
               (format " — errors:%d" (length auto-research-dashboard--errors))
             "")))
    (tabulated-list-print t)))

(defun auto-research-dashboard--external-emit (generation items)
  "Accept external ITEMS for source GENERATION."
  (when (= generation auto-research-dashboard--source-generation)
    (dolist (item items)
      (unless (auto-research-item-p item)
        (user-error "Dashboard provider emitted non-item: %S" item)))
    (setq auto-research-dashboard--items
          (nconc auto-research-dashboard--items items))
    (auto-research-dashboard--render)))

(defun auto-research-dashboard--external-done (generation plugin-id error-text)
  "Mark external PLUGIN-ID source done for GENERATION."
  (when (= generation auto-research-dashboard--source-generation)
    (setq auto-research-dashboard--pending-sources
          (max 0 (1- auto-research-dashboard--pending-sources)))
    (when (and error-text (not (string-empty-p (string-trim error-text))))
      (push (format "%s: %s" plugin-id error-text)
            auto-research-dashboard--errors))
    (auto-research-dashboard--render)))

(defun auto-research-dashboard--start-external-sources ()
  "Start every registered external dashboard provider."
  (let* ((dashboard (current-buffer))
         (generation auto-research-dashboard--source-generation)
         (providers (auto-research-plugin-dashboard-providers)))
    (setq auto-research-dashboard--pending-sources (length providers))
    (dolist (entry providers)
      (let ((plugin-id (car entry))
            (provider (cdr entry)))
        (condition-case error-data
            (funcall
             provider auto-research-dashboard--scope
             (lambda (items)
               (when (buffer-live-p dashboard)
                 (with-current-buffer dashboard
                   (auto-research-dashboard--external-emit generation items))))
             (lambda (&optional error-text)
               (when (buffer-live-p dashboard)
                 (with-current-buffer dashboard
                   (auto-research-dashboard--external-done
                    generation plugin-id error-text)))))
          (error
           (auto-research-dashboard--external-done
            generation plugin-id (error-message-string error-data))))))))

(defun auto-research-dashboard-refresh ()
  "Refresh local research and every enabled external dashboard source."
  (interactive)
  (cl-incf auto-research-dashboard--source-generation)
  (setq auto-research-dashboard--errors nil
        auto-research-dashboard--pending-sources 0)
  (auto-research-dashboard--scan-local)
  (auto-research-dashboard--render)
  (auto-research-dashboard--start-external-sources))

(defun auto-research-dashboard-at-point ()
  "Return the research item at point."
  (let ((id (tabulated-list-get-id)))
    (or (seq-find (lambda (item)
                    (string= id (auto-research-dashboard--item-id item)))
                  auto-research-dashboard--items)
        (user-error "No research item on this row"))))

(defun auto-research-dashboard-context ()
  "Return plugin context for the selected dashboard row."
  (let ((item (auto-research-dashboard-at-point)))
    (list :source 'dashboard
          :item item
          :file (auto-research-item-file item)
          :project (auto-research-item-project item))))

(defun auto-research-dashboard-run-plugin-action ()
  "Run an integration action for the selected research item."
  (interactive)
  (auto-research-plugin-run-action (auto-research-dashboard-context)))

(defun auto-research-dashboard-open ()
  "Open the selected research document."
  (interactive)
  (let ((item (auto-research-dashboard-at-point)))
    (if-let ((open-function (auto-research-item-open-function item)))
        (funcall open-function item)
      (let ((file (auto-research-item-file item)))
        (unless file
          (user-error "Research item has no open handler"))
        (find-file file)
        (when (fboundp 'auto-research-document-mode)
          (auto-research-document-mode 1))
        (when-let ((project (auto-research-item-project item)))
          (setq-local auto-research-document-project
                      (auto-research-project-id project)))))))

(defun auto-research-dashboard-search (text)
  "Filter dashboard rows by TEXT."
  (interactive (list (read-string "Research search (empty clears): "
                                  auto-research-dashboard--search)))
  (setq auto-research-dashboard--search (string-trim text))
  (auto-research-dashboard--render))

(defun auto-research-dashboard-select-project ()
  "Restrict the dashboard to one configured local project."
  (interactive)
  (let ((project (auto-research-project-read-project "Show research project: ")))
    (setq auto-research-dashboard--scope (auto-research-project-id project))
    (auto-research-dashboard-refresh)))

(defun auto-research-dashboard-all-projects ()
  "Reset dashboard scope to all configured projects and external sources."
  (interactive)
  (setq auto-research-dashboard--scope 'all)
  (auto-research-dashboard-refresh))

(defun auto-research-dashboard-errors ()
  "Show errors reported by local/external dashboard sources or decisions."
  (interactive)
  (with-output-to-temp-buffer "*Auto Research Errors*"
    (princ (if auto-research-dashboard--errors
               (string-join (reverse auto-research-dashboard--errors) "\n\n")
             "No errors."))))

(defun auto-research-dashboard--mode-map ()
  "Build the dashboard keymap."
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map tabulated-list-mode-map)
    (define-key map (kbd "RET") #'auto-research-dashboard-open)
    (define-key map (kbd "a") #'auto-research-approve)
    (define-key map (kbd "R") #'auto-research-reject)
    (define-key map (kbd "n") #'auto-research-new)
    (define-key map (kbd "p") #'auto-research-dashboard-select-project)
    (define-key map (kbd "A") #'auto-research-dashboard-all-projects)
    (define-key map (kbd "x") #'auto-research-dashboard-run-plugin-action)
    (define-key map (kbd "/") #'auto-research-dashboard-search)
    (define-key map (kbd "g") #'auto-research-dashboard-refresh)
    (define-key map (kbd "e") #'auto-research-dashboard-errors)
    map))

(defvar auto-research-dashboard-mode-map
  (auto-research-dashboard--mode-map))

(define-derived-mode auto-research-dashboard-mode tabulated-list-mode "Auto-Research"
  "Major mode for the unified auto-research control plane."
  (setq tabulated-list-format
        [("Approval" 12 t)
         ("Project" 30 t)
         ("Lifecycle" 18 t)
         ("Title" 48 t)
         ("Path" 0 t)]
        tabulated-list-padding 2
        tabulated-list-sort-key '("Project" . nil))
  (tabulated-list-init-header))

(defun auto-research-dashboard (&optional scope)
  "Open the unified research dashboard.
SCOPE is `all' or a configured local project identifier."
  (interactive)
  (let ((buffer (get-buffer-create "*Auto Research*")))
    (with-current-buffer buffer
      (auto-research-dashboard-mode)
      (setq auto-research-dashboard--scope (or scope 'all))
      (auto-research-dashboard-refresh))
    (pop-to-buffer buffer)))

(provide 'auto-research-dashboard)
;;; auto-research-dashboard.el ends here
