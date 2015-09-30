;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; -*- emacs-lisp -*-
;; Perforce support.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; When using vc-p4 we don't want the p4 module to take ownership of
;; files
(require 'p4)
(setq p4-do-find-file nil
      p4-executable "/usr/local/bin/p4")
;; (require 'vc-p4)

;; I use p4-diff-all-opened more than p4-diff2
(global-unset-key "\C-xpd")
(global-set-key "\C-xpd" 'p4-diff-all-opened)

;; my own, asynchronous p4 "mode".
(setq tkj-p4-out-buffer "*tkj-p4-out*")

(defun tkj-p4-edit()
  (interactive)
  (shell-command
   (concat "p4 edit " (buffer-file-name))
   (get-buffer-create tkj-p4-out-buffer)
   (get-buffer-create tkj-p4-out-buffer))
  (revert-buffer))

(defun tkj-p4-revert()
  (interactive)
  (shell-command
   (concat "p4 revert " (buffer-file-name) "&")
   (get-buffer-create tkj-p4-out-buffer)
   (get-buffer-create tkj-p4-out-buffer))
  (revert-buffer))

(defun tkj-get-diff-buffer()
  (interactive)
  (get-buffer-create "tkj-p4-diff-buffer"))

(defun tkj-p4-diff()
  (interactive)
  (shell-command
   (concat "p4 diff " (buffer-file-name) "&")
   (tkj-get-diff-buffer)
   (tkj-get-diff-buffer))
;;  (save-excursion
;;      (switch-to-buffer (tkj-get-diff-buffer))
;;      (diff-mode))
  )

(defun tkj-p4-opened()
  (interactive)
  (shell-command
   "p4 opened &"
   (get-buffer-create tkj-p4-out-buffer)
   (get-buffer-create tkj-p4-out-buffer)))

(defun tkj-p4-filelog()
  (interactive)
  (shell-command
   (concat "p4 filelog " (buffer-file-name) " &")
   (get-buffer-create tkj-p4-out-buffer)
   (get-buffer-create tkj-p4-out-buffer)))

(defun tkj-p4-annotate()
  (interactive)
  (shell-command
   (concat "p4 annotate -i -c " (buffer-file-name) " &")
   (get-buffer-create tkj-p4-out-buffer)
   (get-buffer-create tkj-p4-out-buffer)))

(defun tkj-p4-submit()
  (interactive)
  (shell-command
   (concat "xterm -e 'p4 submit " default-directory "...' &")
   (get-buffer-create tkj-p4-out-buffer)
   (get-buffer-create tkj-p4-out-buffer)))

;; (global-set-key "\C-xpe" 'tkj-p4-edit)
;; (global-set-key "\C-xpf" 'tkj-p4-filelog)
;; (global-set-key "\C-xpr" 'tkj-p4-revert)
;; (global-set-key "\C-xp=" 'tkj-p4-diff)
;; (global-set-key "\C-xpo" 'tkj-p4-opened)
;; (global-set-key "\C-xpS" 'tkj-p4-submit)
;; (global-set-key "\C-xpV" 'tkj-p4-annotate)
