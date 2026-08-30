;;; auto-research-plugin.el --- Extension API for auto-research -*- lexical-binding: t; -*-

(require 'cl-lib)
(require 'seq)

(cl-defstruct (auto-research-plugin
               (:constructor auto-research-plugin-create))
  "A standalone integration registered with emacs-auto-research.

ID is a stable symbol/string.
PROJECT-PROVIDER is a zero-argument function returning project plists in the
same shape as `auto-research-projects'.
DOCUMENT-PREDICATE, when non-nil, receives FILE and may claim/recognize it.
ACTIONS is a function receiving the current research item/file and returning
extra action descriptors for an integration-specific UI.
AFTER-DECISION is called with FILE, STATE and PROJECT after a human decision is
persisted.  Hooks may observe or enqueue work, but must not rewrite the human
approval decision behind the core package's back."
  id project-provider document-predicate actions after-decision)

(defvar auto-research--plugins nil
  "Registered `auto-research-plugin' objects.")

(defun auto-research-register-plugin (plugin)
  "Register PLUGIN and return it.

Registration replaces an existing plugin with the same ID.  The core package
never requires any plugin by name, so workflow engines such as Consortium can
integrate without becoming dependencies of emacs-auto-research."
  (unless (auto-research-plugin-p plugin)
    (signal 'wrong-type-argument (list 'auto-research-plugin plugin)))
  (let ((id (auto-research-plugin-id plugin)))
    (setq auto-research--plugins
          (cons plugin
                (seq-remove
                 (lambda (existing)
                   (equal id (auto-research-plugin-id existing)))
                 auto-research--plugins))))
  plugin)

(defun auto-research-unregister-plugin (id)
  "Unregister plugin ID."
  (setq auto-research--plugins
        (seq-remove
         (lambda (plugin)
           (equal id (auto-research-plugin-id plugin)))
         auto-research--plugins)))

(defun auto-research-plugins ()
  "Return registered plugins in deterministic registration order."
  (reverse auto-research--plugins))

(defun auto-research-plugin-project-plists ()
  "Return project plists contributed by registered plugins."
  (cl-loop for plugin in (auto-research-plugins)
           for provider = (auto-research-plugin-project-provider plugin)
           when provider
           append (or (funcall provider) nil)))

(defun auto-research-plugin-after-decision (file state project)
  "Notify registered plugins after STATE is persisted for FILE in PROJECT."
  (dolist (plugin (auto-research-plugins))
    (when-let ((callback (auto-research-plugin-after-decision plugin)))
      (funcall callback file state project))))

(provide 'auto-research-plugin)
;;; auto-research-plugin.el ends here
