(setq sh-basic-offset 2
      sh-indentation 2)

;; snippets, please
(add-hook 'sh-mode-hook 'yas-minor-mode)

;; on the fly syntax checking
(add-hook 'sh-mode-hook 'flycheck-mode)

;; show git changes in the gutter
(add-hook 'sh-mode-hook 'git-gutter+-mode)

(defun tkj-shunit-disable-all-tests()
  (interactive)
  (save-excursion
    (goto-char 0)
    (while (search-forward "test_" nil t)
      (replace-match "not_test_" nil t))))

(defun tkj-shunit-enable-all-tests()
  (interactive)
  (save-excursion
    (goto-char 0)
    (while (search-forward "not_test_" nil t)
      (replace-match "test_" nil t))))
