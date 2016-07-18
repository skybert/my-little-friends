(setq httpd-port 9999)

(setq web-mode-code-indent-offset 2
      web-mode-markup-indent-offset 2
      web-mode-css-indent-offset 2
      )

(add-hook 'web-mode-hook
          '(lambda ()
             (yas-minor-mode)
             ))

(defun tkj-tidy-up-css()
  (interactive)
  (goto-char 0)
  (replace-string ";" ";
")
  (goto-char 0)
  (replace-string "{" "
{
")
  (goto-char 0)
  (replace-string "}" "
}
")
  (indent-region (point-min) (point-max)))
(global-set-key (kbd "C-x s") 'tkj-tidy-up-css)
