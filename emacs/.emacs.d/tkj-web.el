(setq httpd-port 9999)

(setq web-mode-code-indent-offset 2
      web-mode-markup-indent-offset 2
      web-mode-css-indent-offset 2
      )

(add-hook 'web-mode-hook
          '(lambda ()
             (yas-minor-mode)
             ))
