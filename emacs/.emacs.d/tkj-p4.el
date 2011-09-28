;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; -*- emacs-lisp -*-
;; Perforce support.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; When using vc-p4 we don't want the p4 module to take ownership of
;; files
(require 'p4)
(setq p4-do-find-file nil
      p4-executable "/usr/local/bin/p4")
(require 'vc-p4)
