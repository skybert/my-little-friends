
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Java flymake support using the Eclipse compiler
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'flymake)

;; setting the class path
(if (eq system-type 'darwin)
    (setq tkj-dir "/mnt/debian/home/torstein/src/p4/")
  (setq tkj-dir "/home/torstein/src/p4/")
)

(setq tkj-classpath
      (concat
       tkj-dir "projects/poc/memento/memento-ws/target/classes"
       ":/opt/tomcat-dev1/webapps/webservice/WEB-INF/classes"
       ))

(defun tkj-read-jar(file)
  (setq tkj-classpath
        (concat tkj-classpath ":" file)))

(require 'find-lisp)
(mapc 'tkj-read-jar (find-lisp-find-files "/usr/lib/jvm/java-6-sun/jre/lib" "\\.jar$"))

(defun flymake-java-ecj-init ()
  (let* ((temp-file   (flymake-init-create-temp-buffer-copy
                       'jde-ecj-create-temp-file))
         (local-file  (file-relative-name
                       temp-file
                       (file-name-directory buffer-file-name))))

    ;; if you've downloaded ecj from eclipse.org, then use these two lines:
    ;;    (list "java" (list "-jar" 
    ;;                       "/usr/share/java/ecj.jar"

    ;; if installing it with DEB packages,or by some other means
    ;; giving you the ecj BASH script front end, simply use this line
    ;; instead:
    (list "ecj" (list 
                 "-Xemacs" 
                 "-d" "/dev/null" 
                 "-source" "1.6"
                 "-target" "1.6"
                 "-sourcepath" (car tkj-sourcepath)
                 "-classpath" 
                 tkj-classpath
                 local-file))))

(defun flymake-java-ecj-cleanup ()
  "Cleanup after `flymake-java-ecj-init' -- delete temp file and dirs."
  (flymake-safe-delete-file flymake-temp-source-file-name)
  (when flymake-temp-source-file-name
    (flymake-safe-delete-directory
     (file-name-directory flymake-temp-source-file-name))))

(defun jde-ecj-create-temp-file (file-name prefix)
  "Create the file FILE-NAME in a unique directory in the temp directory."
  (file-truename (expand-file-name
                  (file-name-nondirectory file-name)
                  (expand-file-name  (int-to-string (random)) 
                                     (flymake-get-temp-dir)))))

(push '(".+\\.java$" flymake-java-ecj-init 
        flymake-java-ecj-cleanup) flymake-allowed-file-name-masks)

(push '("\\(.*?\\):\\([0-9]+\\): error: \\(.*?\\)\n" 1 2 nil 2 3
        (6 compilation-error-face)) compilation-error-regexp-alist)

(push '("\\(.*?\\):\\([0-9]+\\): warning: \\(.*?\\)\n" 1 2 nil 1 3
        (6 compilation-warning-face)) compilation-error-regexp-alist)

(defun credmp/flymake-display-err-minibuf () 
  "Displays the error/warning for the current line in the minibuffer"
  (interactive)
  (let* ((line-no             (flymake-current-line-no))
         (line-err-info-list  (nth 0 (flymake-find-err-info
                                      flymake-err-info line-no)))
         (count               (length line-err-info-list))
         )
    (while (> count 0)
      (when line-err-info-list
        (let* ((file       (flymake-ler-file (nth (1- count)
                                                  line-err-info-list)))
               (full-file  (flymake-ler-full-file (nth (1- count)
                                                       line-err-info-list)))
               (text (flymake-ler-text (nth (1- count) line-err-info-list)))
               (line       (flymake-ler-line (nth (1- count)
                                                  line-err-info-list))))
          (message "[%s] %s" line text)
          )
        )
      (setq count (1- count)))))


(defun tkj-show-previous-error ()
  (interactive)
  (flymake-goto-prev-error)
  (credmp/flymake-display-err-minibuf))

(defun tkj-show-next-error ()
  (interactive)
  (flymake-goto-next-error)
  (credmp/flymake-display-err-minibuf))

(setq tkj-sourcepath (list ""))

(defun my-java-flymake-mode-hook ()
  (flymake-mode)
  (define-key c-mode-base-map "\C-c\C-n" 'tkj-show-next-error)
  (define-key c-mode-base-map "\C-c\C-p" 'tkj-show-previous-error)
)
(add-hook 'c-mode-common-hook 'my-java-flymake-mode-hook)
