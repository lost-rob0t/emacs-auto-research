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
  project file title lifecycle approval)

(defvar-local auto-research-dashboard--items nil)
(defvar-local auto-research-dashboard--scope 'all)
(defvar-local auto-research-dashboard--search "")

(defun auto-research-dashboard--item-id (item)
  "Return stable dashboard identity for ITEM."
  (format "%s::%s"
          (auto-research-project-id (auto-research-item-project item))
          (file-truename (auto-research-item-file item))))

(defun auto-research-dashboard--read-file (project file)
  "Read FILE into an `auto-research-item' for PROJECT."
  (with-temp-buffer
    (insert-file-contents file)
    (let ((content (buffer-string)))
      (auto-research-item--create
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

(defun auto-research-dashboard--scan ()
  "Scan research files for the current dashboard scope."
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
               (list (auto-research-project-name (auto-research-item-project item))
                     (auto-research-project-id (auto-research-item-project item))
                     (auto-research-item-title item)
                     (auto-research-item-lifecycle item)
                     (auto-research-item-approval item)
                     (auto-research-item-file item))
               "\n"))))
        (string-match-p (regexp-quote needle) haystack))))

(defun auto-research-dashboard--row (item)
  "Return tabulated row for ITEM."
  (list
   (auto-research-dashboard--item-id item)
   (vector
    (auto-research-item-approval item)
    (auto-research-project-name (auto-research-item-project item))
    (auto-research-item-lifecycle item)
    (auto-research-item-title item)
    (file-relative-name
     (auto-research-item-file item)
     (auto-research-project-root (auto-research-item-project item))))))

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
          (format "Auto Research — %s — %d/%d  [RET open] [a approve] [R reject] [n new] [p project] [A all] [x plugin] [/ search] [g refresh]"
                  (auto-research-dashboard--scope-label)
                  (length visible)
                  (length auto-research-dashboard--items)))
    (tabulated-list-print t)))

(defun auto-research-dashboard-refresh ()
  "Refresh the unified research dashboard."
  (interactive)
  (auto-research-dashboard--scan)
  (auto-research-dashboard--render))

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
    (find-file (auto-research-item-file item))
    (when (fboundp 'auto-research-document-mode)
      (auto-research-document-mode 1))
    (setq-local auto-research-document-project
                (auto-research-project-id (auto-research-item-project item)))))

(defun auto-research-dashboard-search (text)
  "Filter dashboard rows by TEXT."
  (interactive (list (read-string "Research search (empty clears): "
                                  auto-research-dashboard--search)))
  (setq auto-research-dashboard--search (string-trim text))
  (auto-research-dashboard--render))

(defun auto-research-dashboard-select-project ()
  "Restrict the dashboard to one configured project."
  (interactive)
  (let ((project (auto-research-project-read-project "Show research project: ")))
    (setq auto-research-dashboard--scope (auto-research-project-id project))
    (auto-research-dashboard-refresh)))

(defun auto-research-dashboard-all-projects ()
  "Reset dashboard scope to all configured projects."
  (interactive)
  (setq auto-research-dashboard--scope 'all)
  (auto-research-dashboard-refresh))

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
    map))

(defvar auto-research-dashboard-mode-map
  (auto-research-dashboard--mode-map))

(define-derived-mode auto-research-dashboard-mode tabulated-list-mode "Auto-Research"
  "Major mode for the unified auto-research control plane."
  (setq tabulated-list-format
        [("Approval" 12 t)
         ("Project" 20 t)
         ("Lifecycle" 18 t)
         ("Title" 48 t)
         ("Path" 0 t)]
        tabulated-list-padding 2
        tabulated-list-sort-key '("Project" . nil))
  (tabulated-list-init-header))

(defun auto-research-dashboard (&optional scope)
  "Open the unified research dashboard.
SCOPE is `all' or a configured project identifier."
  (interactive)
  (let ((buffer (get-buffer-create "*Auto Research*")))
    (with-current-buffer buffer
      (auto-research-dashboard-mode)
      (setq auto-research-dashboard--scope (or scope 'all))
      (auto-research-dashboard-refresh))
    (pop-to-buffer buffer)))

(provide 'auto-research-dashboard)
;;; auto-research-dashboard.el ends here
