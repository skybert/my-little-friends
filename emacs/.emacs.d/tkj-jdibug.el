;; -*- emacs-lisp -*-
(setq src "/home/torstein/src")
(setq jdibug-connect-hosts (quote ("localhost:5005"))
      jdibug-use-jdee-source-paths nil
      jdibug-source-paths
      (list
       (concat src "/content-engine/engine/engine-core/src/main/java")
       (concat src "/content-engine/engine/engine-webservice/src/main/java")
       "/usr/lib/jvm/jdk-8-oracle-x64/src"
       )
)

(add-to-list 'load-path "/usr/local/src/jdibug/build/jdibug-0.7")
(require 'jdibug)
(require 'jdibug-ui)

(defun my-jdibug-mode-hook ()
  ;; IDEA style debug short cuts
  (define-key c-mode-base-map [ (f7) ] 'jdibug-step-into)
  (define-key c-mode-base-map [ (f8) ] 'jdibug-step-over)
  (define-key c-mode-base-map [ (f9) ] 'jdibug-step-out)
  (define-key c-mode-base-map [ (f10) ] 'jdibug-resume)
  (global-set-key "\C-\M-b" 'jdibug-connect)
)
(add-hook 'c-mode-common-hook 'my-jdibug-mode-hook)
