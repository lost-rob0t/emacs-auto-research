;;; auto-research-plugin.el --- Extension API for auto-research -*- lexical-binding: t; -*-

(require 'cl-lib)
(require 'seq)
(require 'subr-x)

(cl-defstruct (auto-research-plugin
               (:constructor auto-research-plugin-create))
  "A standalone integration registered with emacs-auto-research.

ID is a stable symbol/string.
PROJECT-PROVIDER is a zero-argument function returning project plists in the
same shape as `auto-research-projects'.
DOCUMENT-PREDICATE, when non-nil, receives FILE and may claim/recognize it.
ACTIONS is a function receiving a context plist and returning action plists.
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
           append
           (condition-case error-data
               (or (funcall provider) nil)
             (error
              (display-warning
               'auto-research
               (format "Plugin %s project provider failed: %s"
                       (auto-research-plugin-id plugin)
                       (error-message-string error-data))
               :warning)
              nil))))

(defun auto-research-plugin-after-decision (file state project)
  "Notify registered plugins after STATE is persisted for FILE in PROJECT.
Plugin failures are warnings because the human decision is already durable."
  (dolist (plugin (auto-research-plugins))
    (when-let ((callback (auto-research-plugin-after-decision plugin)))
      (condition-case error-data
          (funcall callback file state project)
        (error
         (display-warning
          'auto-research
          (format "Plugin %s failed after %s decision for %s: %s"
                  (auto-research-plugin-id plugin) state file
                  (error-message-string error-data))
          :warning))))))

(defun auto-research-plugin-actions-for-context (context)
  "Return normalized plugin actions available for CONTEXT.

Each action provider returns plists with at least :id, :label and :command.
:command receives CONTEXT.  The returned action is annotated with :plugin."
  (cl-loop for plugin in (auto-research-plugins)
           for provider = (auto-research-plugin-actions plugin)
           when provider
           append
           (mapcar
            (lambda (action)
              (unless (and (plist-get action :id)
                           (stringp (plist-get action :label))
                           (functionp (plist-get action :command)))
                (user-error "Plugin %s returned an invalid action: %S"
                            (auto-research-plugin-id plugin) action))
              (let ((copy (copy-sequence action)))
                (plist-put copy :plugin (auto-research-plugin-id plugin))
                copy))
            (or (funcall provider context) nil))))

(defun auto-research-plugin-run-action (context)
  "Prompt for and run a plugin action for CONTEXT."
  (let* ((actions (auto-research-plugin-actions-for-context context))
         (choices
          (mapcar
           (lambda (action)
             (cons (format "%s: %s"
                           (plist-get action :plugin)
                           (plist-get action :label))
                   action))
           actions)))
    (unless choices
      (user-error "No plugin actions are available here"))
    (let* ((choice (completing-read "Research plugin action: " choices nil t))
           (action (cdr (assoc choice choices))))
      (funcall (plist-get action :command) context))))

(provide 'auto-research-plugin)
;;; auto-research-plugin.el ends here
