;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Themeing
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq custom-theme-load-path
      '(custom-theme-directory t "$HOME/.emacs.d/themes"
      load-cust)

(defun tkj-load-zenburn()
  (interactive)
  (load-theme 'zenburn t))

