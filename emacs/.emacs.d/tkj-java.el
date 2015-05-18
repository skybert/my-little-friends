;; -*- emacs-lisp -*-
(defun tkj-insert-serial-version-uuid()
  (interactive)
  (insert "private static final long serialVersionUID = 1L;"))

(defun tkj-eclim-maven-run-quick-package()
  (interactive)
  (eclim-maven-run "-o -q -DskipTests package"))

(defun my-java-mode-hook ()
  (auto-fill-mode)
  (gtags-mode)
  ;;  (flyspell-prog-mode)
  ;;  (flymake-mode)
  (subword-mode)
  (smartscan-mode)

  (define-key c-mode-base-map "\C-\M-j" 'tkj-insert-serial-version-uuid)
  (define-key c-mode-base-map "\C-m" 'c-context-line-break)
  (define-key c-mode-base-map (kbd "<f2>") 'eclim-problems)

  ;; Setting up a number of Java related shortcuts to mimic IDEA.
  (define-key c-mode-base-map "\C-\M-g" 'eclim-java-find-declaration)
  (define-key c-mode-base-map "\C-\M-o" 'eclim-java-import-organize)
  (define-key c-mode-base-map "\C-q" 'eclim-java-show-documentation-for-current-element)
  (define-key c-mode-base-map "\M-i" 'eclim-java-implement) ;; IDEA is C-i
  (define-key c-mode-base-map (kbd "<M-RET>") 'eclim-problems-correct)
  (define-key c-mode-base-map (kbd "<M-f7>") 'eclim-java-find-references)
  (define-key c-mode-base-map (kbd "<S-f6>") 'eclim-java-refactor-rename-symbol-at-point)
  (define-key c-mode-base-map (kbd "<S-f7>") 'gtags-find-tag-from-here)
  (define-key c-mode-base-map (kbd "<C-f9>") 'tkj-eclim-maven-run-quick-package)

  ;; Fix indentation for anonymous classes
  (c-set-offset 'substatement-open 0)
  (if (assoc 'inexpr-class c-offsets-alist)
      (c-set-offset 'inexpr-class 0))

  ;; Indent arguments on the next line as indented body.
  (c-set-offset 'arglist-intro '+))
(add-hook 'java-mode-hook 'my-java-mode-hook)

(defun tkj-default-code-style-hook()
  (setq c-basic-offset 2
        c-label-offset 0
        indent-tabs-mode nil
        compile-command "cd ~/src/drifting/jms && mvn -q -o -DskipTests package"
        require-final-newline nil))
(add-hook 'java-mode-hook 'tkj-default-code-style-hook)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Flymake settings
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'flymake)
(setq flymake-log-level -1) ;; 3 is debug

;; On the fly checkstyle & pmd checking
(defun my-flymake-init ()
  (list "my-java-flymake-checks"
        (list (flymake-init-create-temp-buffer-copy
               'flymake-create-temp-with-folder-structure))))
;; (add-to-list 'flymake-allowed-file-name-masks
;;             '("\\.java$" my-flymake-init flymake-simple-cleanup))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Interface to eclipse via eclim
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'eclim)
(global-eclim-mode)

;; Variables
(setq eclim-auto-save t
      eclim-executable "/opt/eclipse/eclim"
      eclimd-wait-for-process nil
      eclimd-default-workspace "~/src/workspace-eclim"
      eclim-use-yasnippet nil
      eclim-autoupdate-problems nil
      eclim-print-debug-messages nil
      help-at-pt-display-when-idle t
      help-at-pt-timer-delay 0.3
      )

;; Call the help framework with the settings above & activate
;; eclim-mode
(help-at-pt-set-timer)

;; Hook eclim up with auto complete mode
(require 'auto-complete-config)
(ac-config-default)
(require 'ac-emacs-eclim-source)
(ac-emacs-eclim-config)

;; restore the window configuration after running certain eclim commands
;; (add-hook 'eclim-problems-mode-hook 'winner-undo)
