;;; auto-research-test.el --- Tests for emacs-auto-research -*- lexical-binding: t; -*-

(require 'ert)
(require 'auto-research)
(require 'auto-research-plugin)
(require 'auto-research-github)

(defconst auto-research-test--partial
  "#+title: Example\n#+status: REVIEW\n#+approval_schema: prolog-rlm.research-approval.v1\n#+approval_state: PENDING\n\n* Body\nKeep me.\n")

(ert-deftest auto-research-repairs-partial-approval-metadata ()
  (let ((updated (auto-research-metadata-decide
                  auto-research-test--partial
                  "APPROVED" "human" "human:test"
                  "abc" "def" "2026-08-29T20:00:00-04:00")))
    (should (auto-research-metadata-canonical-p updated))
    (should (string-match-p "#+approval_schema: auto-research.approval.v1" updated))
    (should (string-match-p "#+approval_state: APPROVED" updated))
    (should (string-match-p "Keep me\\." updated))))

(ert-deftest auto-research-repair-is-idempotent ()
  (let* ((once (auto-research-metadata-normalize auto-research-test--partial))
         (twice (auto-research-metadata-normalize once)))
    (should (equal once twice))))

(ert-deftest auto-research-refuses-conflicting-duplicates ()
  (let ((content
         "#+title: Example\n#+status: REVIEW\n#+approval_state: PENDING\n#+approval_state: APPROVED\n"))
    (should-error (auto-research-metadata-normalize content) :type 'user-error)))

(ert-deftest auto-research-project-scan-keeps-project-identity ()
  (let* ((dir-a (make-temp-file "auto-research-a" t))
         (dir-b (make-temp-file "auto-research-b" t))
         (auto-research--plugins nil)
         (auto-research-projects
          `((:id a :root ,dir-a :research-root "research")
            (:id b :root ,dir-b :research-root "research"))))
    (unwind-protect
        (progn
          (make-directory (expand-file-name "research" dir-a))
          (make-directory (expand-file-name "research" dir-b))
          (dolist (dir (list dir-a dir-b))
            (with-temp-file (expand-file-name "research/same.org" dir)
              (insert "#+title: Same\n#+status: REVIEW\n")))
          (should (= 1 (length (auto-research-project-research-files
                                (auto-research-project-by-id "a")))))
          (should (= 1 (length (auto-research-project-research-files
                                (auto-research-project-by-id "b"))))))
      (delete-directory dir-a t)
      (delete-directory dir-b t))))

(ert-deftest auto-research-plugin-contributes-projects-without-core-coupling ()
  (let* ((dir (make-temp-file "auto-research-plugin" t))
         (auto-research-projects nil)
         (auto-research--plugins nil))
    (unwind-protect
        (progn
          (auto-research-register-plugin
           (auto-research-plugin-create
            :id 'example-workflow
            :project-provider
            (lambda ()
              `((:id plugin-project
                 :name "Plugin Project"
                 :root ,dir
                 :research-root "research")))))
          (let ((project (auto-research-project-by-id "plugin-project")))
            (should project)
            (should (string= "Plugin Project" (auto-research-project-name project)))))
      (delete-directory dir t))))

(ert-deftest auto-research-plugin-observes-human-decision ()
  (let ((auto-research--plugins nil)
        observed)
    (auto-research-register-plugin
     (auto-research-plugin-create
      :id 'observer
      :after-decision
      (lambda (file state project)
        (setq observed (list file state project)))))
    (auto-research-plugin-notify-after-decision "/tmp/research.org" "APPROVED" nil)
    (should (equal (car observed) "/tmp/research.org"))
    (should (equal (cadr observed) "APPROVED"))
    (should-not (caddr observed))))

(ert-deftest auto-research-directly-opened-file-is-recognized ()
  (let* ((dir (make-temp-file "auto-research-open" t))
         (research (expand-file-name "research" dir))
         (file (expand-file-name "item.org" research))
         (auto-research--plugins nil)
         (auto-research-projects
          `((:id direct :root ,dir :research-root "research"))))
    (unwind-protect
        (progn
          (make-directory research)
          (with-temp-file file
            (insert "#+title: Direct\n#+status: REVIEW\n"))
          (should (string=
                   "direct"
                   (auto-research-project-id
                    (auto-research-document-project-for-file file)))))
      (delete-directory dir t))))

(ert-deftest auto-research-dashboard-accepts-async-external-items ()
  (let ((auto-research-projects nil)
        (auto-research--plugins nil))
    (auto-research-register-plugin
     (auto-research-plugin-create
      :id 'remote-test
      :dashboard-provider
      (lambda (_scope emit done)
        (funcall emit
                 (list
                  (auto-research-dashboard-make-item
                   :id "remote::one"
                   :display-project "owner/repo"
                   :path "research/ONE.org"
                   :title "Remote One"
                   :lifecycle "REVIEW"
                   :approval "PENDING"
                   :open-function #'ignore)))
        (funcall done nil))))
    (with-temp-buffer
      (auto-research-dashboard-mode)
      (setq auto-research-dashboard--scope 'all)
      (auto-research-dashboard-refresh)
      (should (= 1 (length auto-research-dashboard--items)))
      (should (= 0 auto-research-dashboard--pending-sources))
      (should (equal "remote::one"
                     (auto-research-dashboard--item-id
                      (car auto-research-dashboard--items)))))))

(ert-deftest auto-research-github-filters-research-paths ()
  (should (auto-research-github--candidate-path-p "research/RESEARCH-001.org"))
  (should (auto-research-github--candidate-path-p "roam/research/topic/item.org"))
  (should-not (auto-research-github--candidate-path-p "docs/item.org"))
  (should-not (auto-research-github--candidate-path-p "research/index.org")))

(ert-deftest auto-research-github-builds-pending-remote-item ()
  (let* ((auto-research-github-show-legacy-default nil)
         (content
          "#+title: Remote\n#+status: REVIEW\n#+approval_schema: auto-research.approval.v1\n#+approval_state: PENDING\n#+approval_actor: NONE\n#+approval_evidence: NONE\n#+approval_base_commit: NONE\n#+approval_base_blob: NONE\n#+approval_decided_at: NONE\n")
         (item (auto-research-github--remote-item
                "owner/repo" "main" "research/REMOTE.org" "blob" content)))
    (should item)
    (should (equal "github::owner/repo::research/REMOTE.org"
                   (auto-research-item-id item)))
    (should (eq #'auto-research-github-decide-item
                (auto-research-item-decision-function item)))))

(ert-deftest auto-research-github-legacy-items-are-opt-in ()
  (let ((content "#+title: Legacy\n#+status: REVIEW\n"))
    (let ((auto-research-github-show-legacy-default nil))
      (should-not
       (auto-research-github--remote-item
        "owner/repo" "main" "research/LEGACY.org" "blob" content)))
    (let ((auto-research-github-show-legacy-default t))
      (should
       (auto-research-github--remote-item
        "owner/repo" "main" "research/LEGACY.org" "blob" content)))))

(provide 'auto-research-test)
;;; auto-research-test.el ends here
