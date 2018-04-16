;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Torstein's Python setup for Emacs
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setq python-indent 2)
(add-hook 'python-mode-hook 'flycheck-mode)
(add-hook 'python-mode-hook 'yas-minor-mode)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Currently (2016-07-19), elpy seems better than anaconda. The latter
;; of which has problems on Emacs 25.1.50.
(use-package elpy
  ;; TODO tkj: remove highlight-indentation from elpy-modules
  ;;
  ;; elpy-modules is a variable defined in ‘elpy.el’.  Its value is
  ;; (elpy-module-sane-defaults elpy-module-company elpy-module-eldoc
  ;; elpy-module-flymake elpy-module-highlight-indentation
  ;; elpy-module-pyvenv elpy-module-yasnippet elpy-module-django)
  :config
  (elpy-enable))

;; Override default elpy key binding(s)
(define-key elpy-mode-map (kbd "<C-return>") 'elpy-company-backend)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Eldoc
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(add-hook 'python-mode-hook 'eldoc-mode)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Syntax checking
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; (add-hook 'python-mode-hook 'flymake-python-pyflakes-load)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Multi Python environment support
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; (setq python-shell-virtualenv-path "/home/torstein/src/atelier/src/main/python/pyenv")
;; (pyenv-mode)

(defun projectile-pyenv-mode-set ()
  "Set pyenv version matching project name.
Version must be already installed."
  (pyenv-mode-set (projectile-project-name)))

;; (add-hook 'projectile-switch-project-hook 'projectile-pyenv-mode-set)
