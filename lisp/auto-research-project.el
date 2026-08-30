;;; auto-research-project.el --- Research project registry -*- lexical-binding: t; -*-

(require 'cl-lib)
(require 'project)
(require 'seq)
(require 'subr-x)

(defgroup auto-research nil
  "Generic research control plane for Emacs."
  :group 'tools)

(defcustom auto-research-projects nil
  "Configured research projects.

Each entry is a plist with at least :id and :root.  :name defaults to
:id.  :research-root defaults to \"research\" and may be absolute or
relative to :root.

Example:

  '((:id prolog-rlm
     :name \"Prolog RLM\"
     :root \"~/src/prolog-rlm\"
     :research-root \"research\"))"
  :type '(repeat plist)
  :group 'auto-research)

(cl-defstruct (auto-research-project
               (:constructor auto-research-project--create))
  id name root research-root)

(defun auto-research-project--normalize-id (value)
  "Return VALUE as a stable string project identifier."
  (cond
   ((symbolp value) (symbol-name value))
   ((stringp value) value)
   (t (user-error "Research project :id must be a symbol or string: %S" value))))

(defun auto-research-project-from-plist (plist)
  "Build an `auto-research-project' from PLIST."
  (let* ((id (auto-research-project--normalize-id (plist-get plist :id)))
         (root-value (plist-get plist :root))
         (root (and root-value
                    (file-name-as-directory
                     (expand-file-name root-value))))
         (research-value (or (plist-get plist :research-root) "research"))
         (research-root
          (and root
               (file-name-as-directory
                (if (file-name-absolute-p research-value)
                    (expand-file-name research-value)
                  (expand-file-name research-value root))))))
    (unless root
      (user-error "Research project %s is missing :root" id))
    (auto-research-project--create
     :id id
     :name (or (plist-get plist :name) id)
     :root root
     :research-root research-root)))

(defun auto-research-project-list ()
  "Return normalized configured research projects."
  (mapcar #'auto-research-project-from-plist auto-research-projects))

(defun auto-research-project-by-id (id)
  "Return configured project matching ID, or nil."
  (let ((needle (auto-research-project--normalize-id id)))
    (seq-find (lambda (project)
                (string= needle (auto-research-project-id project)))
              (auto-research-project-list))))

(defun auto-research-project-at-directory (&optional directory)
  "Return the configured research project containing DIRECTORY.
Prefer the deepest matching root."
  (let* ((directory (file-truename
                     (file-name-as-directory
                      (expand-file-name (or directory default-directory)))))
         (matches
          (seq-filter
           (lambda (project)
             (let ((root (file-truename (auto-research-project-root project))))
               (file-in-directory-p directory root)))
           (auto-research-project-list))))
    (car (sort matches
               (lambda (a b)
                 (> (length (auto-research-project-root a))
                    (length (auto-research-project-root b))))))))

(defun auto-research-project-current ()
  "Return the configured project associated with the current project.el project."
  (let* ((project (project-current nil))
         (root (and project (project-root project))))
    (and root (auto-research-project-at-directory root))))

(defun auto-research-project-research-files (project)
  "Return Org research files belonging to PROJECT."
  (let ((root (auto-research-project-research-root project)))
    (if (not (file-directory-p root))
        nil
      (sort (directory-files-recursively root "\\.org\\'" nil nil nil)
            #'string-lessp))))

(defun auto-research-project-read-project (&optional prompt)
  "Interactively select a configured project.
PROMPT defaults to \"Research project: \"."
  (let* ((projects (auto-research-project-list))
         (choices (mapcar (lambda (project)
                            (cons (format "%s (%s)"
                                          (auto-research-project-name project)
                                          (auto-research-project-id project))
                                  project))
                          projects)))
    (unless choices
      (user-error "No `auto-research-projects' are configured"))
    (cdr (assoc (completing-read (or prompt "Research project: ")
                                 choices nil t)
                choices))))

(provide 'auto-research-project)
;;; auto-research-project.el ends here
