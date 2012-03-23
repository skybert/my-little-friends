;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Themeing
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq custom-theme-load-path
      '(custom-theme-directory t
                               "/usr/local/src/djcb-elisp/themes" 
                               "/mnt/debian/usr/local/src/djcb-elisp/themes")
      load-cust)

(defun tkj-load-zenburn()
  (interactive)
  (load-theme 'zenburn t))

