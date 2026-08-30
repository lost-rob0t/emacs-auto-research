;;; auto-research-approval.el --- Research decision UX -*- lexical-binding: t; -*-

(require 'subr-x)
(require 'auto-research-metadata)
(require 'auto-research-dashboard)
(require 'auto-research-plugin)
(require 'auto-research-project)

(defcustom auto-research-default-approval-actor user-full-name
  "Default human actor recorded for research decisions."
  :type 'string
  :group 'auto-research)

(defcustom auto-research-default-approval-evidence "human:emacs-auto-research"
  "Default durable evidence string recorded for research decisions."
  :type 'string
  :group 'auto-research)

(defvar-local auto-research-document-project nil)

(defun auto-research-approval--target-file ()
  "Return the local research file targeted by the current context."
  (cond
   ((derived-mode-p 'auto-research-dashboard-mode)
    (or (auto-research-item-file (auto-research-dashboard-at-point))
        (user-error "Selected research item is remote")))
   ((and buffer-file-name (derived-mode-p 'org-mode))
    buffer-file-name)
   (t (user-error "Point is not on a research item and this is not an Org research file"))))

(defun auto-research-approval--target-project (file)
  "Return configured project associated with FILE, if any."
  (or (and auto-research-document-project
           (auto-research-project-by-id auto-research-document-project))
      (auto-research-project-at-directory (file-name-directory file))))

(defun auto-research-document-context ()
  "Return plugin context for the currently open research document."
  (let ((file (auto-research-approval--target-file)))
    (list :source 'document
          :file file
          :project (auto-research-approval--target-project file))))

(defun auto-research-run-plugin-action ()
  "Run an integration action for the current research document."
  (interactive)
  (auto-research-plugin-run-action (auto-research-document-context)))

(defun auto-research-approval--git-value (file &rest args)
  "Run Git ARGS for FILE and return trimmed stdout, or \"NONE\" on failure."
  (let ((default-directory (file-name-directory file)))
    (with-temp-buffer
      (if (zerop (apply #'process-file "git" nil t nil args))
          (let ((value (string-trim (buffer-string))))
            (if (string-empty-p value) "NONE" value))
        "NONE"))))

(defun auto-research-approval--base-metadata (file)
  "Return best-effort Git base metadata for FILE."
  (let ((commit (auto-research-approval--git-value file "rev-parse" "HEAD"))
        (blob "NONE"))
    (when (not (string= commit "NONE"))
      (let* ((root (auto-research-approval--git-value file "rev-parse" "--show-toplevel"))
             (path (and (not (string= root "NONE"))
                        (file-relative-name file root))))
        (when path
          (setq blob (auto-research-approval--git-value
                      file "rev-parse" (format "HEAD:%s" path))))))
    (list commit blob)))

(defun auto-research-approval--write-decision (file state actor evidence)
  "Repair approval metadata in FILE and record STATE."
  (let* ((original (with-temp-buffer
                     (insert-file-contents file)
                     (buffer-string)))
         (base (auto-research-approval--base-metadata file))
         (updated (auto-research-metadata-decide
                   original state actor evidence (car base) (cadr base))))
    (unless (string= original updated)
      (with-temp-file file
        (insert updated)))
    updated))

(defun auto-research-approval--read-identity ()
  "Prompt for the human actor and durable evidence string."
  (let ((actor (read-string "Decision maker: " auto-research-default-approval-actor))
        (evidence (read-string "Approval evidence: " auto-research-default-approval-evidence)))
    (when (string-empty-p (string-trim actor))
      (user-error "Decision maker must be nonempty"))
    (when (string-empty-p (string-trim evidence))
      (user-error "Approval evidence must be nonempty"))
    (list actor evidence)))

(defun auto-research-approval--decide-external (item state actor evidence)
  "Run external decision handler for ITEM."
  (let ((handler (auto-research-item-decision-function item))
        (dashboard (current-buffer)))
    (unless handler
      (user-error "Selected remote research item does not support decisions"))
    (when (auto-research-item-busy item)
      (user-error "Decision already running"))
    (setf (auto-research-item-busy item) t)
    (auto-research-dashboard--render)
    (funcall
     handler item state actor evidence
     (lambda (ok message-text)
       (when (buffer-live-p dashboard)
         (with-current-buffer dashboard
           (setf (auto-research-item-busy item) nil)
           (if ok
               (progn
                 (message "%s" (or message-text "Research decision delivered"))
                 (auto-research-dashboard-refresh))
             (push (or message-text "External research decision failed")
                   auto-research-dashboard--errors)
             (auto-research-dashboard--render)
             (message "Research decision failed: %s"
                      (or message-text "unknown error")))))))))

(defun auto-research-decide (state)
  "Record research decision STATE in the current dashboard/file context."
  (pcase-let ((`(,actor ,evidence) (auto-research-approval--read-identity)))
    (if (derived-mode-p 'auto-research-dashboard-mode)
        (let ((item (auto-research-dashboard-at-point)))
          (if (auto-research-item-decision-function item)
              (auto-research-approval--decide-external item state actor evidence)
            (let* ((file (or (auto-research-item-file item)
                             (user-error "Research item has no decision handler")))
                   (project (auto-research-item-project item)))
              (auto-research-approval--write-decision file state actor evidence)
              (auto-research-plugin-after-decision file state project)
              (auto-research-dashboard-refresh)
              (message "%s research: %s" state (file-name-nondirectory file)))))
      (let* ((file (auto-research-approval--target-file))
             (project (auto-research-approval--target-project file)))
        (auto-research-approval--write-decision file state actor evidence)
        (auto-research-plugin-after-decision file state project)
        (when (and buffer-file-name (file-equal-p file buffer-file-name))
          (revert-buffer :ignore-auto :noconfirm))
        (message "%s research: %s" state (file-name-nondirectory file))))))

(defun auto-research-approve ()
  "Approve the selected or currently open research document.

This command is context-sensitive.  Local documents are updated in place.
External dashboard items may provide a decision handler such as the optional
GitHub approval-PR backend."
  (interactive)
  (auto-research-decide "APPROVED"))

(defun auto-research-reject ()
  "Reject the selected or currently open research document."
  (interactive)
  (auto-research-decide "REJECTED"))

(defun auto-research-repair ()
  "Deterministically repair approval metadata in the current local research file."
  (interactive)
  (let* ((file (auto-research-approval--target-file))
         (content (with-temp-buffer
                    (insert-file-contents file)
                    (buffer-string)))
         (updated (auto-research-metadata-normalize content)))
    (unless (string= content updated)
      (with-temp-file file (insert updated)))
    (when (and buffer-file-name (file-equal-p file buffer-file-name))
      (revert-buffer :ignore-auto :noconfirm))
    (when (derived-mode-p 'auto-research-dashboard-mode)
      (auto-research-dashboard-refresh))
    (message "Research metadata canonical: %s" (file-name-nondirectory file))))

(defvar auto-research-document-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-a") #'auto-research-approve)
    (define-key map (kbd "C-c C-r") #'auto-research-reject)
    (define-key map (kbd "C-c C-x") #'auto-research-run-plugin-action)
    map)
  "Keymap for `auto-research-document-mode'.")

(define-minor-mode auto-research-document-mode
  "Minor mode for research documents opened from emacs-auto-research.

It deliberately uses ordinary Emacs bindings.  Evil/Doom integration is an
optional compatibility layer, not a package requirement."
  :lighter " Research"
  :keymap auto-research-document-mode-map)

(provide 'auto-research-approval)
;;; auto-research-approval.el ends here
