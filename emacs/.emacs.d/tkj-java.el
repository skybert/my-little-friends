;; -*- emacs-lisp -*-

(defun tkj-insert-serial-version-uuid()
  (interactive)
  (insert "private static final long serialVersionUID = 1L;"))

(defun my-java-mode-hook ()
  (auto-fill-mode)
  (gtags-mode)
  (subword-mode)
  (yas-minor-mode)
  (idle-highlight-mode)
  (git-gutter+-mode)
  (flyspell-prog-mode)

  (define-key c-mode-base-map (kbd "C-M-j") 'tkj-insert-serial-version-uuid)
  (define-key c-mode-base-map (kbd "C-m") 'c-context-line-break)
  (define-key c-mode-base-map (kbd "S-<f7>") 'gtags-find-tag-from-here)

  ;; Fix indentation for anonymous classes
  (c-set-offset 'substatement-open 0)
  (if (assoc 'inexpr-class c-offsets-alist)
      (c-set-offset 'inexpr-class 0))

  ;; Indent arguments on the next line as indented body.
  (c-set-offset 'arglist-intro '++))
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
;; Highlighting gud
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun tkj-java-debug-highlighting()
  (interactive)
  (insert "monitor locals\n")
  (highlight-regexp "^[a-zA-Z].* =" "hi-green")
  (highlight-phrase "com.escenic" "hi-red-b")
  (highlight-phrase "neo." "hi-red-b")
  )
(add-hook 'gud-mode-hook 'tkj-java-debug-highlighting)

(defun tkj-java-compile-and-run-buffer()
  """
  Compiles the current Java buffer and runs it.
  """
  (interactive)
  (compile
   (concat
    "javac "
    (buffer-name)
    " && "
    "java "
    (replace-regexp-in-string ".java" "" (buffer-name)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; I often want to explicitly load big Emacs modules to minimize the
;; start up time
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun tkj-load-jdibug ()
  (interactive)
  (load "~/.emacs.d/tkj-jdibug.el"))

(defun tkj-load-eclim ()
  (interactive)
  (load "~/.emacs.d/tkj-java-eclim.el"))

(defun tkj-load-meghanada ()
  (interactive)
  (load "~/.emacs.d/tkj-java-meghanada.el"))


(tkj-load-meghanada)
