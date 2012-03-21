;; -*- emacs-lisp -*-

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Make Emacs understand Ant's and Maven's output
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'compile)

;; (setq compilation-error-regexp-alist
;;   (append (list
;;           ;; works for jikes
;;           '("^\\s-*\\[[^]]*\\]\\s-*\\(.+\\):\\([0-9]+\\):\\([0-9]+\\):[0-9]+:[0-9]+:" 1 2 3)
;;           ;; works for javac
;;           '("^\\s-*\\[[^]]*\\]\\s-*\\(.+\\):\\([0-9]+\\):" 1 2)
;;           ;; works for maven 2.x
;;           '("^\\(.*\\):\\[\\([0-9]*\\),\\([0-9]*\\)\\]" 1 2 3)
;;           ;; works for maven 3.x
;;           '("^\\(\\[ERROR\\] \\)?\\(/[^:]+\\):\\[\\([0-9]+\\),\\([0-9]+\\)\\]" 2 3 4)
;;           '("^\\(\\[WARNING\\] \\)?\\(/[^:]+\\):\\[\\([0-9]+\\),\\([0-9]+\\)\\]" 2 3 4)
;;           )
;;          compilation-error-regexp-alist))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; jtags
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(add-to-list 'load-path "/usr/local/src/jtags/src/lisp")
(autoload 'jtags-mode "jtags" "Toggle jtags mode." t)
(add-hook 'java-mode-hook 'jtags-mode)

