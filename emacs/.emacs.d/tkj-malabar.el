;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; -*- emacs-lisp -*-
;; malabar
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; For Emacs 23.1 and earlier
;; (setq cedet-emacs-min-version "some")
;; (require 'cedet)
;; (semantic-load-enable-minimum-features) ;; or enable more if you wish

;; Emacs 23.2 or later. Enable more if you wish
(setq semantic-default-submodes '(global-semantic-idle-scheduler-mode
                                  global-semanticdb-minor-mode
                                  global-semantic-idle-summary-mode
                                  global-semantic-mru-bookmark-mode))
(semantic-mode 1)

;; (if (equal "darwin" system-type)
;;    (setq malabar-groovy-lib-dir "/mnt/debian/usr/local/src/malabar-mode/target/malabar-1.5-SNAPSHOT/lib")

(setq malabar-groovy-lib-dir "/usr/local/src/malabar-mode/target/malabar-1.5-SNAPSHOT/lib")
;; (setq load-path
;;      (append (list"/mnt/debian/usr/local/src/malabar-mode/src/main/lisp"
;;                   "/usr/local/src/malabar-mode/src/main/lisp"
;;              load-path))
(require 'malabar-mode)

(setq
      malabar-load-source-from-sibling-projects t
      malabar-extra-source-locations
      '("/usr/lib/jvm/java-6-sun/src"
        "/mnt/debian/usr/lib/jvm/java-6-sun/src"
        "$HOME/src/p4/escenic/engine/branches/5.2/engine-syndication/src/main/java"
        "$HOME/src/p4/escenic/engine/branches/5.2/engine-core/src/main/java"
        "$HOME/src/p4/escenic/engine/branches/5.2/engine-presentation/src/main/java"
        "$HOME/src/p4/escenic/engine/branches/5.2/engine-indexer-webservice/src/main/java"
        "$HOME/src/p4/escenic/engine/branches/5.2/engine-servletsupport/src/main/java"
        "$HOME/src/p4/escenic/engine/branches/5.2/engine-taglib/src/main/java"
        "$HOME/src/p4/escenic/indexer/branches/1.2/indexer-webapp/src/main/java"                                      
        "/usr/local/src/apache-tomcat-6.0.16-src/java")
      )

(add-to-list 'auto-mode-alist '("\\.java\\'" . malabar-mode))

(defun my-java-malabar-mode-hook ()
  ;; IDEA default for jump to source
  (define-key c-mode-base-map "\C-\M-g" 'malabar-jump-to-thing)
  (global-set-key "\M-n" 'semantic-ia-complete-symbol)
  )
(add-hook 'c-mode-common-hook 'my-java-malabar-mode-hook)


;; Compiling the file on save makes malabar display the errors in the
;; Java source code.
(add-hook 'malabar-mode-hook
          (lambda () 
            (add-hook 'after-save-hook 'malabar-compile-file-silently
                      nil t)))

