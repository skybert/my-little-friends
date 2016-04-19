(setq sh-basic-offset 2
      sh-indentation 2)

;; snippets, please
(add-hook 'sh-mode-hook 'yas-minor-mode)

;; on the fly syntax checking
(add-hook 'sh-mode-hook 'flycheck-mode)
