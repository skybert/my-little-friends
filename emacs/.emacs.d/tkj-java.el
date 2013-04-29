;; -*- emacs-lisp -*-
(defun tkj-insert-serial-version-uuid()
  (interactive)
  (insert "private static final long serialVersionUID = 1L;")
)

(defun my-c-mode-hook ()
  (setq c-basic-offset 4
        c-label-offset 0
        indent-tabs-mode nil
        compile-command "cd ~/src/DocEngine-develop && mvn -o -DskipTests package"
        require-final-newline nil)
  (auto-fill-mode)
  (gtags-mode)

  (define-key c-mode-base-map "\C-\M-\\" 'eclim-java-format)
  (define-key c-mode-base-map "\C-\M-j" 'tkj-insert-serial-version-uuid)
  (define-key c-mode-base-map "\C-c\C-i" 'yas/expand)
  (define-key c-mode-base-map "\C-m" 'c-context-line-break)

  ;; Setting up a number of Java related shortcuts to mimic IDEA.
  (define-key c-mode-base-map "\C-\M-g" 'eclim-java-find-declaration)
  (define-key c-mode-base-map "\C-\M-o" 'eclim-java-import-organize)
  (define-key c-mode-base-map "\C-q" 'eclim-java-show-documentation-for-current-element)
  (define-key c-mode-base-map "\M-i" 'eclim-java-implement) ;; IDEA is C-i
  (define-key c-mode-base-map (kbd "<M-RET>") 'eclim-problems-correct)
  (define-key c-mode-base-map (kbd "<M-f7>") 'eclim-java-find-references)
  (define-key c-mode-base-map (kbd "<S-f6>") 'eclim-java-refactor-rename-symbol-at-point)

  ;; fix indentation for anonymous classes
  (c-set-offset 'substatement-open 0)
  (if (assoc 'inexpr-class c-offsets-alist)
      (c-set-offset 'inexpr-class 0))

  (subword-mode)
  )
(add-hook 'c-mode-common-hook 'my-c-mode-hook)
