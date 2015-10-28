(defun tkj-go-before-save-hook()
  "Applies gofmt on save for .go files"
  (interactive)
  (when (eq major-mode 'go-mode)
    (save-excursion
      (gofmt))))

(add-hook 'before-save-hook 'tkj-go-before-save-hook)
