;;; auto-research-doom.el --- Optional Doom compatibility -*- lexical-binding: t; -*-

(require 'auto-research-dashboard)
(require 'auto-research-approval)

(when (featurep 'evil)
  (evil-set-initial-state 'auto-research-dashboard-mode 'motion))

(provide 'auto-research-doom)
;;; auto-research-doom.el ends here
