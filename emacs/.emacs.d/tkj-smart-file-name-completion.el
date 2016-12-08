;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; IDO & filecache: smart file name completion
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'ido)

;; Improved flex matching
(require 'flx-ido)
;; Vertical completion menu
(require 'ido-vertical-mode)

(setq ido-everywhere nil
      ido-enable-flex-matching t
      ido-create-new-buffer 'always
      ido-file-extensions-order '(".java" ".js" ".el" ".xml")
      ido-use-filename-at-point 'guess
      ido-use-faces t
      ido-vertical-indicator "→"
      ido-vertical-show-count t
      )
(ido-mode 'buffer)

(ido-vertical-mode)
(ido-vertical-mode 1)
(setq ido-vertical-define-keys 'C-n-C-p-up-and-down)

(defun tkj-switch-buffer()
  (interactive)
  ;;    (ido-vertical-mode 0)
  (ido-switch-buffer)
  ;; ido-vertical-mode never gets turned on
  ;;    (ido-vertical-mode 1)
  )

;; If not using ido-vertical-mode, make the minibuff stay still,
;; i.e. never change height, set this to nil.
;; (setq resize-mini-windows 'grow-only)

;; IDO support pretty much everwhere, including eclim-java-implement
(require 'ido-ubiquitous)
(ido-ubiquitous)

(global-set-key (kbd "C-x b") 'tkj-switch-buffer)

;; Enhanced M-x
(require 'smex)
(global-set-key (kbd "M-x") 'smex)

;; General project support
(require 'projectile)
(projectile-global-mode)
(setq projectile-enable-caching t
      projectile-globally-ignored-directories
      (append (list "target" "output" "node_modules" "reveal.js" "venv")
              projectile-globally-ignored-directories)
      projectile-globally-ignored-file-suffixes '("iml" "ipr" "classpath" "tkj")
      projectile-tags-command "/usr/bin/ctags -Re -f \"%s\" %s"
      projectile-mode-line '(:eval (format " [%s]" (projectile-project-name)))
      )
(global-set-key (kbd "C-;") 'projectile-find-file)

;; Show search hits of strings in current buffer
;; http://oremacs.com/2015/01/26/occur-dwim/
(defun occur-dwim ()
  "Call `occur' with a sane default."
  (interactive)
  (push (if (region-active-p)
            (buffer-substring-no-properties
             (region-beginning)
             (region-end))
          (let ((sym (thing-at-point 'symbol)))
            (when (stringp sym)
              (regexp-quote sym))))
        regexp-history)
  (call-interactively 'occur))
(global-set-key (kbd "M-s o") 'occur-dwim)

(global-set-key (kbd "C-,") 'imenu)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Sometimes, for instance when working on HUGE code bases - or on
;; windows, it's better to use a good old file cache (I generate mine
;; with ../../bash/create-emacs-file-cache).
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun tkj-load-filecache()
  (interactive)
  (require 'filecache)
  (setq tkj-file-cache-file "~/.emacs.d/file_cache.el")

  (defun file-cache-read-cache-from-file ()
    "Clear `file-cache-alist' and read cache from FILE.
  The file cache can be saved to a file using
  `file-cache-save-cache-to-file'."
    (interactive)
    ;;  (interactive "fFile: ")
    (file-cache-clear-cache)
    (save-excursion
      (set-buffer (find-file-noselect tkj-file-cache-file))
      (beginning-of-buffer)
      (setq file-cache-alist (read (current-buffer)))))

  (defun file-cache-save-cache-to-file ()
    (interactive)
    "Save contents of `file-cache-alist' to FILE.
For later retrieval using `file-cache-read-cache-from-file'"
    ;;  (interactive "FFile: ")
    (with-temp-file (expand-file-name tkj-file-cache-file)
      (prin1 file-cache-alist (current-buffer))))

  ;; write the file_cache file when exiting emacs
  ;; (add-hook 'kill-emacs-hook
  ;;           'file-cache-save-cache-to-file)

  ;; and read it again when starting emacs
  (file-cache-read-cache-from-file)

  ;; add files to the file cache when killing them
  (defun file-cache-add-this-file ()
    (and buffer-file-name
         (file-exists-p buffer-file-name)
         (file-cache-add-file buffer-file-name)))
  ;; (add-hook 'kill-buffer-hook 'file-cache-add-this-file)

  ;; Using ido to open files from file name cache

  (defun file-cache-ido-find-file (file)
    "Using ido, interactively open file from file cache'.
First select a file, matched using ido-switch-buffer against the contents
in `file-cache-alist'. If the file exist in more than one
directory, select directory. Lastly the file is opened."
    (interactive (list (file-cache-ido-read "File: "
                                            (mapcar
                                             (lambda (x)
                                               (car x))
                                             file-cache-alist))))
    (let* ((record (assoc file file-cache-alist)))
      (find-file
       (expand-file-name
        file
        (if (= (length record) 2)
            (car (cdr record))
          (file-cache-ido-read
           (format "Find %s in dir: " file) (cdr record)))))))

  (defun file-cache-ido-read (prompt choices)
    (let ((ido-make-buffer-list-hook
           (lambda ()
             (setq ido-temp-list choices))))
      (ido-read-buffer prompt)))

  (global-set-key "\C-cf" 'file-cache-ido-find-file)
  )
