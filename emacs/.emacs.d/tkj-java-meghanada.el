(use-package meghanada
  :bind
  (:map meghanada-mode-map
        (("C-M-o" . meghanada-optimize-import)
         ("C-M-t" . meghanada-import-all)
         )))
(defun tkj-java-meghanda-mode-hook ()
  (meghanada-mode)
  (flycheck-mode))
(add-hook 'java-mode-hook 'tkj-java-meghanda-mode-hook)
