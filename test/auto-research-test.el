;;; auto-research-test.el --- Tests for emacs-auto-research -*- lexical-binding: t; -*-

(require 'ert)
(require 'auto-research)

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

(provide 'auto-research-test)
;;; auto-research-test.el ends here
