;;; auto-research-doom.el --- Optional Doom compatibility -*- lexical-binding: t; -*-

(require 'auto-research-dashboard)
(require 'auto-research-approval)

(when (featurep 'evil)
  (evil-set-initial-state 'auto-research-dashboard-mode 'motion)
  (dolist (state '(normal motion))
    (evil-define-key state auto-research-dashboard-mode-map
      (kbd "RET") #'auto-research-dashboard-open
      (kbd "a") #'auto-research-approve
      (kbd "R") #'auto-research-reject
      (kbd "n") #'auto-research-new
      (kbd "p") #'auto-research-dashboard-select-project
      (kbd "A") #'auto-research-dashboard-all-projects
      (kbd "/") #'auto-research-dashboard-search
      (kbd "g") #'auto-research-dashboard-refresh)
    (evil-define-key state auto-research-document-mode-map
      (kbd "a") #'auto-research-approve
      (kbd "R") #'auto-research-reject)))

(provide 'auto-research-doom)
;;; auto-research-doom.el ends here
