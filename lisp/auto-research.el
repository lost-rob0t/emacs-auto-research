;;; auto-research.el --- Generic research control plane -*- lexical-binding: t; -*-
;; Package-Requires: ((emacs "29.1"))
;; Version: 0.1.0

;;; Commentary:
;;
;; A generic, project-scoped Emacs UI for research documents and human gates.
;; The same UX is used to browse research and to approve an open document.

(require 'cl-lib)
(require 'org)
(require 'subr-x)
(require 'auto-research-project)
(require 'auto-research-metadata)
(require 'auto-research-dashboard)
(require 'auto-research-approval)

(defcustom auto-research-new-file-name-function #'auto-research-default-file-name
  "Function called with TITLE to derive a new research filename."
  :type 'function
  :group 'auto-research)

(defun auto-research-default-file-name (title)
  "Return a conservative Org filename for research TITLE."
  (concat
   (replace-regexp-in-string
    "^-\\|-$" ""
    (replace-regexp-in-string "[^[:alnum:]]+" "-" (downcase title)))
   ".org"))

(defun auto-research--new-document-content (title seed)
  "Return canonical initial research content for TITLE and SEED."
  (let ((base (string-join
               (delq nil
                     (list
                      (format "#+title: %s" title)
                      "#+status: DRAFT"
                      ""
                      "* Research question"
                      (unless (string-empty-p seed) seed)
                      ""
                      "* Findings"
                      ""
                      "* Sources"
                      ""
                      "* Decision notes"
                      ""))
               "\n")))
    (auto-research-metadata-normalize base)))

(defun auto-research--destination-project ()
  "Return the project used for creating research from the current context."
  (cond
   ((derived-mode-p 'auto-research-dashboard-mode)
    (if (eq auto-research-dashboard--scope 'all)
        (auto-research-project-read-project "Create research in: ")
      (or (auto-research-project-by-id auto-research-dashboard--scope)
          (auto-research-project-read-project "Create research in: "))))
   ((auto-research-project-at-directory default-directory))
   (t (auto-research-project-read-project "Create research in: "))))

(defun auto-research-new (title seed)
  "Create a new research document with TITLE and optional SEED."
  (interactive
   (list (read-string "Research title: ")
         (read-string "Initial question/context (optional): ")))
  (let* ((project (auto-research--destination-project))
         (research-root (auto-research-project-research-root project))
         (name (funcall auto-research-new-file-name-function title))
         (file (expand-file-name name research-root)))
    (make-directory research-root t)
    (when (file-exists-p file)
      (user-error "Research file already exists: %s" file))
    (with-temp-file file
      (insert (auto-research--new-document-content title seed)))
    (when (derived-mode-p 'auto-research-dashboard-mode)
      (auto-research-dashboard-refresh))
    (find-file file)
    (auto-research-document-mode 1)
    (setq-local auto-research-document-project (auto-research-project-id project))
    (message "Created research: %s" file)))

(defun auto-research-current-project ()
  "Open the control plane scoped to the current configured project."
  (interactive)
  (let ((project (or (auto-research-project-current)
                     (auto-research-project-at-directory default-directory))))
    (unless project
      (user-error "Current directory is not inside a configured research project"))
    (auto-research-dashboard (auto-research-project-id project))))

;;;###autoload
(defun auto-research ()
  "Open the unified research control plane showing all configured projects."
  (interactive)
  (auto-research-dashboard 'all))

;; Doom users usually have Evil loaded, while vanilla Emacs users often do not.
;; Keep Evil entirely optional and activate compatibility only when it exists.
(with-eval-after-load 'evil
  (require 'auto-research-evil nil t))

(provide 'auto-research)
;;; auto-research.el ends here
