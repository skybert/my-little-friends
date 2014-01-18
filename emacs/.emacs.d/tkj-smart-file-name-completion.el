;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; IDO & filecache: smart file name completion
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'ido)
(require 'flx-ido)

(setq ido-everywhere nil
      ido-enable-flex-matching t
      ido-create-new-buffer 'always
      ido-file-extensions-order '(".java" ".js" ".el" ".xml")
      ido-use-filename-at-point 'guess
      ido-use-faces t
      )
(ido-mode 'buffer)

(require 'ido-vertical-mode)
(ido-vertical-mode)

(require 'projectile)
(projectile-global-mode)
(setq projectile-enable-caching nil)

(global-set-key "\C-cf" 'projectile-find-file)

