;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Themeing
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(add-to-list 'custom-theme-load-path
             "$HOME/.emacs.d/themes")

;; Add the user-contributed repository

(setq package-archives '(("ELPA" . "http://tromey.com/elpa/")
                         ("marmalade" . "http://marmalade-repo.org/packages/")))

;; loading packages installed via the emacs24 package repositories
;; here, < version 22 & 23, the package are installed loaded on the
;; system level.
(let ((default-directory "~/.emacs.d/elpa"))
  (normal-top-level-add-subdirs-to-load-path))
(require 'auto-complete)
(require 'yasnippet)
(require 'mic-paren)
(require 'magit)
(require 'yaml-mode)
(require 'twittering-mode)

;; twittering 
(setq twittering-use-master-password t)

(defun tkj-load-zenburn()
  (interactive)
  (load-theme 'zenburn t))



