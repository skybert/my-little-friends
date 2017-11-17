(require 'meghanada)
(defun tkj-java-meghanda-mode-hook ()
  (meghanada-mode)
  (flycheck-mode))
(add-hook 'java-mode-hook 'tkj-java-meghanda-mode-hook)

;; Make shortcuts the same as in IDEA
(define-key meghanada-mode-map (kbd "C-M-o") 'meghanada-optimize-import)
(define-key meghanada-mode-map (kbd "C-M-t") 'meghanada-import-all)
