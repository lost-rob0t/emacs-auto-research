;;; auto-research-metadata.el --- Canonical research metadata -*- lexical-binding: t; -*-

(require 'cl-lib)
(require 'seq)
(require 'subr-x)

(defcustom auto-research-approval-schema "auto-research.approval.v1"
  "Canonical approval schema written by emacs-auto-research."
  :type 'string
  :group 'auto-research)

(defcustom auto-research-legacy-approval-schemas
  '("prolog-rlm.research-approval.v1"
    "adard.research-approval.v1")
  "Legacy approval schemas accepted for deterministic migration."
  :type '(repeat string)
  :group 'auto-research)

(defconst auto-research-metadata--approval-fields
  '("approval_schema"
    "approval_state"
    "approval_actor"
    "approval_evidence"
    "approval_base_commit"
    "approval_base_blob"
    "approval_decided_at"))

(defconst auto-research-metadata--states '("PENDING" "APPROVED" "REJECTED"))

(defun auto-research-metadata--keyword-values (content keyword)
  "Return every Org keyword value for KEYWORD in CONTENT, case-insensitively."
  (with-temp-buffer
    (insert content)
    (goto-char (point-min))
    (let ((case-fold-search t)
          values)
      (while (re-search-forward
              (format "^#\\+%s:[ \\t]*\\(.*\\)$" (regexp-quote keyword))
              nil t)
        (push (string-trim (match-string-no-properties 1)) values))
      (nreverse values))))

(defun auto-research-metadata-keyword (content keyword)
  "Return the unique value of KEYWORD in CONTENT, or nil.
Signal when multiple distinct values exist."
  (let ((values (delete-dups
                 (auto-research-metadata--keyword-values content keyword))))
    (cond
     ((null values) nil)
     ((= (length values) 1) (car values))
     (t (user-error "Conflicting #+%s metadata: %s"
                    keyword (string-join values ", "))))))

(defun auto-research-metadata-title (content &optional fallback)
  "Return research title from CONTENT, or FALLBACK."
  (or (auto-research-metadata-keyword content "title") fallback "Untitled research"))

(defun auto-research-metadata-lifecycle (content)
  "Return lifecycle #+status from CONTENT."
  (or (auto-research-metadata-keyword content "status") "MISSING"))

(defun auto-research-metadata-approval-state (content)
  "Return normalized approval state from CONTENT.
Legacy documents without approval state return \"LEGACY\"."
  (let ((state (auto-research-metadata-keyword content "approval_state")))
    (if state (upcase state) "LEGACY")))

(defun auto-research-metadata--known-schema-p (schema)
  "Return non-nil when SCHEMA is canonical or explicitly migratable."
  (or (string= schema auto-research-approval-schema)
      (member schema auto-research-legacy-approval-schemas)))

(defun auto-research-metadata--field-values (content)
  "Return an alist of canonical approval field values from CONTENT.
Equivalent duplicates collapse.  Conflicting duplicates signal."
  (mapcar (lambda (field)
            (cons field (auto-research-metadata-keyword content field)))
          auto-research-metadata--approval-fields))

(defun auto-research-metadata--remove-approval-keywords (content)
  "Remove canonical/legacy approval keyword lines from CONTENT."
  (with-temp-buffer
    (insert content)
    (goto-char (point-min))
    (let ((case-fold-search t)
          (regexp
           (concat "^#\\+"
                   (regexp-opt auto-research-metadata--approval-fields t)
                   ":[ \\t]*.*\\(?:\\n\\|\\'\\)")))
      (while (re-search-forward regexp nil t)
        (replace-match "" t t)))
    (buffer-string)))

(defun auto-research-metadata--canonical-block (state actor evidence base-commit base-blob decided-at)
  "Return canonical approval block text."
  (string-join
   (list
    (format "#+approval_schema: %s" auto-research-approval-schema)
    (format "#+approval_state: %s" state)
    (format "#+approval_actor: %s" actor)
    (format "#+approval_evidence: %s" evidence)
    (format "#+approval_base_commit: %s" base-commit)
    (format "#+approval_base_blob: %s" base-blob)
    (format "#+approval_decided_at: %s" decided-at))
   "\n"))

(defun auto-research-metadata--insert-after-status (content block)
  "Insert BLOCK immediately after the unique #+status line in CONTENT."
  (with-temp-buffer
    (insert content)
    (goto-char (point-min))
    (let ((case-fold-search t))
      (unless (re-search-forward "^#\\+status:[ \\t]*.*$" nil t)
        (user-error "Research document has no #+status keyword"))
      (let ((first-end (line-end-position)))
        (when (re-search-forward "^#\\+status:[ \\t]*.*$" nil t)
          (user-error "Research document has multiple #+status keywords"))
        (goto-char first-end)
        (insert "\n" block)))
    (buffer-string)))

(defun auto-research-metadata--validate-values (content)
  "Validate approval values in CONTENT and return nil on success."
  (let* ((schema (auto-research-metadata-keyword content "approval_schema"))
         (state (auto-research-metadata-keyword content "approval_state")))
    (unless schema
      (user-error "Missing #+approval_schema"))
    (unless (string= schema auto-research-approval-schema)
      (user-error "Noncanonical approval schema: %s" schema))
    (unless (member state auto-research-metadata--states)
      (user-error "Invalid approval_state: %s" (or state "missing")))
    (dolist (field auto-research-metadata--approval-fields)
      (let ((values (auto-research-metadata--keyword-values content field)))
        (unless (= (length values) 1)
          (user-error "Canonical #+%s must occur exactly once" field))))
    t))

(defun auto-research-metadata-canonical-p (content)
  "Return non-nil when CONTENT contains one canonical approval block."
  (condition-case nil
      (progn
        (auto-research-metadata--validate-values content)
        (let ((status-pos (with-temp-buffer
                            (insert content)
                            (goto-char (point-min))
                            (let ((case-fold-search t))
                              (re-search-forward "^#\\+status:[ \\t]*.*$" nil t)
                              (line-end-position))))
              (schema-pos (with-temp-buffer
                            (insert content)
                            (goto-char (point-min))
                            (let ((case-fold-search t))
                              (re-search-forward "^#\\+approval_schema:" nil t)
                              (line-beginning-position)))))
          (= (1+ status-pos) schema-pos)))
    (error nil)))

(defun auto-research-metadata-normalize (content &optional state actor evidence base-commit base-blob decided-at)
  "Return CONTENT with a deterministic canonical approval block.

STATE, ACTOR, EVIDENCE, BASE-COMMIT, BASE-BLOB and DECIDED-AT override
existing values when non-nil.  Equivalent duplicate keywords are collapsed.
Conflicting duplicate values are never guessed."
  (let* ((values (auto-research-metadata--field-values content))
         (old-schema (alist-get "approval_schema" values nil nil #'string=))
         (old-state (alist-get "approval_state" values nil nil #'string=))
         (old-actor (alist-get "approval_actor" values nil nil #'string=))
         (old-evidence (alist-get "approval_evidence" values nil nil #'string=))
         (old-commit (alist-get "approval_base_commit" values nil nil #'string=))
         (old-blob (alist-get "approval_base_blob" values nil nil #'string=))
         (old-date (alist-get "approval_decided_at" values nil nil #'string=))
         (state (upcase (or state old-state "PENDING")))
         (actor (or actor old-actor "NONE"))
         (evidence (or evidence old-evidence "NONE"))
         (base-commit (or base-commit old-commit "NONE"))
         (base-blob (or base-blob old-blob "NONE"))
         (decided-at (or decided-at old-date "NONE")))
    (when (and old-schema (not (auto-research-metadata--known-schema-p old-schema)))
      (user-error "Unknown approval schema cannot be migrated safely: %s" old-schema))
    (unless (member state auto-research-metadata--states)
      (user-error "Invalid approval state cannot be migrated safely: %s" state))
    (when (string= state "PENDING")
      (setq actor "NONE"
            evidence "NONE"
            base-commit "NONE"
            base-blob "NONE"
            decided-at "NONE"))
    (let* ((without (auto-research-metadata--remove-approval-keywords content))
           (block (auto-research-metadata--canonical-block
                   state actor evidence base-commit base-blob decided-at))
           (normalized (auto-research-metadata--insert-after-status without block)))
      (auto-research-metadata--validate-values normalized)
      normalized)))

(defun auto-research-metadata-decide (content state actor evidence &optional base-commit base-blob decided-at)
  "Return CONTENT with approval decision STATE recorded canonically."
  (let ((state (upcase state)))
    (unless (member state '("APPROVED" "REJECTED"))
      (user-error "Decision must be APPROVED or REJECTED"))
    (when (string-empty-p (string-trim actor))
      (user-error "Approval actor must be nonempty"))
    (when (string-empty-p (string-trim evidence))
      (user-error "Approval evidence must be nonempty"))
    (auto-research-metadata-normalize
     content state actor evidence
     (or base-commit "NONE")
     (or base-blob "NONE")
     (or decided-at (format-time-string "%Y-%m-%dT%H:%M:%S%:z")))))

(provide 'auto-research-metadata)
;;; auto-research-metadata.el ends here
