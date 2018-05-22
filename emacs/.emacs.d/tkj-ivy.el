(use-package ivy
  :init
  (setq ivy-re-builders-alist '((t . ivy--regex-fuzzy))
        ivy-initial-inputs-alist nil)
  :bind
  (("C-x b" . 'ivy-switch-buffer)))

(use-package counsel
  :bind
  ("C-," . 'counsel-imenu)
  ("C-h f" . 'counsel-describe-function)
  ("C-h v" . 'counsel-describe-variable)
  ("C-x C-f" . 'counsel-find-file)
  )
