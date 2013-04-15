;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; IDO & filecache: smart file name completion
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'ido)

(setq ido-everywhere nil
      ido-enable-flex-matching t
      ido-create-new-buffer 'always
      ido-file-extensions-order '(".java" ".js" ".el")
      ido-use-filename-at-point 'guess
      ido-create-new-buffer 'always
      )

(ido-mode 'buffer)

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
