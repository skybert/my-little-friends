;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                                          ;;
;;           Torstein Krause Johansen's .emacs file                         ;;
;;                                                                          ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setq user-full-name "Torstein Krause Johansen"
      user-mail-address "tkj@conduct.no")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Appearance
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(menu-bar-mode 0)

(when window-system
  (load-theme 'deeper-blue t)
  (set-scroll-bar-mode nil)
  (tool-bar-mode 0)
  (fringe-mode 0))

(setq frame-background-mode nil
      column-number-mode t
      frame-title-format (concat invocation-name "@" (system-name) " {%f}")
      ;; no visible or audible bells, please
      visible-bell nil
      ring-bell-function (lambda nil (message ""))
      show-paren-mode t)

;; saves the buffer/split configuration, makes it un/re-doable.
(winner-mode 1)

;; themes
(add-to-list 'custom-theme-load-path
             "$HOME/.emacs.d/themes")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Shortcuts in all modes
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(global-set-key "\C-xg" 'goto-line)
(global-set-key "\M- " 'hippie-expand)
(global-set-key "\C-\M-f" 'find-file-at-point)
(global-set-key "\C-cn" 'find-dired)
(global-set-key "\C-cN" 'grep-find)
;; minimising Emacs way too many times without wanting to.
(global-unset-key "\C-z")
;; don't write backslashed to indicate continuous lines
(set-display-table-slot standard-display-table 'wrap ?\ )
;; Treat 'y' or <CR> as yes, 'n' as no.
(fset 'yes-or-no-p 'y-or-n-p)
(setq grep-find-command
      "find ~/src/DocEngine -type f | egrep '.(java|xml|html)$' | egrep -v '.(svn|git)' | xargs grep -n -i -e ")

(defun move-line-down ()
  (interactive)
  (let ((col (current-column)))
    (save-excursion
      (forward-line)
      (transpose-lines 1))
    (forward-line)
    (move-to-column col)))

(defun move-line-up ()
  (interactive)
  (let ((col (current-column)))
    (save-excursion
      (forward-line)
      (transpose-lines -1))
    (move-to-column col)))

(global-set-key (kbd "<C-s-down>") 'move-line-down)
(global-set-key (kbd "<C-s-up>") 'move-line-up)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Prefer UTF 8, but don't override current encoding if specified
;; (unless you specify a write hook).
;;
;; This seems to solve the problem of Gnus encoding the message as
;; utf-16be (64 encoded) when replying to something Bangladeshi.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(prefer-coding-system 'utf-8)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Set tabs to 2 spaces, replace all tabs with spaces and remove all
;; trailing white spaces.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq default-tab-width 2)
(setq-default indent-tabs-mode nil)
;; (add-hook 'before-save-hook untabify)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Spell checking
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq ispell-program-name "aspell"
      ispell-list-command "list"
      ispell-dictionary "british")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Pure text settings
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(add-hook 'text-mode-hook
          '(lambda ()
             (auto-fill-mode 1)
             (flyspell-mode)))
(setq longlines-show-hard-newlines t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Loading other general init files
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; load all locally installed packages
(let ((default-directory "/usr/local/src/emacs"))
  (normal-top-level-add-subdirs-to-load-path))

;; Add ELPA packages and emacs-eclim to the load path
(let ((default-directory "~/.emacs.d/elpa"))
  (normal-top-level-add-subdirs-to-load-path))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Emacs package repositories
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq package-archives '(("melpa" . "http://melpa.milkbox.net/packages/")
                         ("marmalade" . "http://marmalade-repo.org/packages/")))

;; loading packages installed via the emacs24 package repositories
;; here
(let ((default-directory "~/.emacs.d/elpa"))
  (normal-top-level-add-subdirs-to-load-path))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Shortcuts available in all modes
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Making the Mac cmd or Windows key useful, simply letting it act as
;; as its Alt cousin when pressed in combination with my most used
;; combinations (yes, I have re-mapped it in xmodmap, but when
;; changing keymaps too often, it goes totally astray and I sometimes
;; just must give in to the mightyness of the Super key).
(global-set-key [ (super backspace) ] 'backward-kill-word)
(global-set-key [ (super b) ] 'backward-word)
(global-set-key [ (super c) ] 'capitalize-word)
(global-set-key [ (super f) ] 'forward-word)
(global-set-key [ (super l) ] 'downcase-word)
(global-set-key [ (super u) ] 'upcase-word)
(global-set-key [ (super v) ] 'scroll-down)
(global-set-key [ (super w) ] 'kill-ring-save)
(global-set-key [ (super x) ] 'execute-extended-command)

(global-unset-key "\C-x\C-c") ;; quitting too often without wanting to
(global-set-key "\C-x\C-c" 'compile) ;; imenu
;; newline and indent (like other editors, even vi, do).
(global-set-key  "\C-m" 'newline-and-indent)
(global-set-key  "\C-o" 'ecb-goto-window-methods)

(define-key query-replace-map [return] 'act)
(define-key query-replace-map [?\C-m] 'act)

;; Make Emacs wrap long lines visually, but not actually (i.e. no
;; extra line breaks are inserted.
(global-visual-line-mode 1)

;; Save & restore sessions, it drives me nuts ... but can be very
;; useful, so I'll keep the setting just for a while
;; (desktop-save-mode 1)

;; Automatically reload files was modified by external program
(global-set-key  [ (f5) ] 'revert-buffer)
(global-auto-revert-mode 1)
(setq revert-without-query (list "\\.png$" "\\.svg$"))

(defun rename-this-buffer-and-file ()
  "Renames current buffer and file it is visiting."
  (interactive)
  (let ((name (buffer-name))
        (filename (buffer-file-name)))
    (if (not (and filename (file-exists-p filename)))
        (error "Buffer '%s' is not visiting a file!" name)
      (let ((new-name (read-file-name "New name: " filename)))
        (cond ((get-buffer new-name)
               (error "A buffer named '%s' already exists!" new-name))
              (t
               (rename-file filename new-name 1)
               (rename-buffer new-name)
               (set-visited-file-name new-name)
               (set-buffer-modified-p nil)
               (message "File '%s' successfully renamed to '%s'" name (file-name-nondirectory new-name))))))))
(global-unset-key "\C-x \C-r")
(global-set-key (kbd "C-x C-r") 'rename-this-buffer-and-file)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Buffers
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Make C-x C-b maximise the buffer list window, this saves two
;; additional shortcuts from the normal behaviour.
(defun tkj-list-buffers()
  (interactive)
  (list-buffers)
  (other-window 1)
  (delete-other-windows))
(global-unset-key "\C-x\C-b")
(global-set-key "\C-x\C-b" 'tkj-list-buffers)

(defun close-all-buffers ()
  (interactive)
  (mapc 'kill-buffer (buffer-list)))

;; make unique buffer names
(require 'uniquify)
(setq uniquify-buffer-name-style 'post-forward
      uniquify-separator ":")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Mail (and news), common to both Gnus and VM
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq gnus-agent-directory "~/mail/agent"
      gnus-article-save-directory "~/mail"
      gnus-cache-directory "~/mail/cache"
      gnus-directory "~/mail"
      gnus-dribble-directory "~/mail/dribble"
      gnus-local-organization "Conduct"
      mail-default-directory "~/mail"
      mail-from-style 'angles
      mail-interactive nil
      mail-self-blind t
      message-directory "~/mail"
)

(autoload 'mu4e "~/.emacs.d/tkj-mu4e.el" t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Display the time on the status line
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq display-time-24hr-format t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Tell emacs to skip backup files
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq make-backup-files nil)
(setq backup-by-copying-when-mismatch t
      backup-by-copying-when-linked t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Setting the general load path for Emacs. This load path is for
;; packages that only have one .el file and hence reside in a
;; directory with other smaller modes.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq load-path
      (append (list (expand-file-name "/usr/local/emacs/")
                    "/usr/share/emacs23/site-lisp/w3m"
                    "/usr/local/src/varnish/varnish-tools/emacs/")
              load-path))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Auto complete
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'auto-complete)
(require 'auto-complete-config)
(add-to-list 'ac-dictionary-directories
             "~/.emacs.d/auto-complete/dict"
             )

(ac-config-default)
(ac-flyspell-workaround)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; CSS mode
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defvar hexcolour-keywords
  '(("#[abcdef[:digit:]]\\{6\\}"
     (0 (put-text-property
         (match-beginning 0)
         (match-end 0)
         'face (list :background
                     (match-string-no-properties 0)))))))

(defun hexcolour-add-to-font-lock ()
  (font-lock-add-keywords nil hexcolour-keywords))
(add-hook 'css-mode-hook 'hexcolour-add-to-font-lock)
(setq css-indent-offset 2)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Associate different modes with different file types.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq auto-mode-alist
      (append
       '(("\\.awk\\'" . awk-mode)
         ("ChangeLog" . change-log-mode)
         ("\\.bashrc\\'" . sh-mode)
         ("\\.bib\\'" . bibtex-mode)
         ("\\.blockdiag\\'" . perl-mode)
         ("\\.c\\'" . c-mode)
         ("\\.cgi\\'" . python-mode)
         ("\\.conf\\'" . conf-mode)
         ("\\.config\\'" . conf-mode)
         ("\\.cpp\\'" . c++-mode)
         ("\\.css\\'" . css-mode)
         ("\\.diff\\'" . diff-mode)
         ("\\.dtd\\'" . dtd-mode)
         ("\\.ebk\\'" . nxml-mode)
         ("\\.el\\'"  . emacs-lisp-mode)
         ("\\.emacs\\'" . emacs-lisp-mode)
         ("\\.es$" . c++-mode)
         ("\\.htm\\'" . html-mode)
         ("\\.html\\'" . nxml-mode)
         ("\\.idl\\'" . c++-mode)
         ("\\.java$" . java-mode)
         ("\\.jbk\\'" . nxml-mode)
         ("\\.js$" . js2-mode)
         ("\\.json$" . js2-mode)
         ("\\.jsp$" . nxml-mode) ;; nxml-mode
         ("\\.jspf$" . nxml-mode) ;; nxml-mode
         ("\\.less\\'" . javascript-mode)
         ("\\.muse$" . planner-mode)
         ("\\.odl\\'" . c++-mode)
         ("\\.org\\'" . org-mode)
         ("\\.pdf\\'" . doc-view-mode)
         ("\\.php\\'" . php-mode)
         ("\\.phtml\\'" . php-mode)
         ("\\.pl\\'" . perl-mode)
         ("\\.properties.template\\'" . conf-mode)
         ("\\.properties\\'" . conf-mode)
         ("\\.puppet\\'" . puppet-mode)
         ("\\.py$" . python-mode)
         ("\\.py\\'" . python-mode)
         ("\\.sed\\'" . sh-mode)
         ("\\.sh\\'" . sh-mode)
         ("\\.shtml\\'" . nxml-mode)
         ("\\.sql\\'" . sql-mode)
         ("\\.targets$" . nxml-mode)
         ("\\.tex\\'" . latex-mode)
         ("\\.text\\'" . text-mode)
         ("\\.tld.*\\'" . nxml-mode)
         ("\\.txt\\'" . text-mode)
         ("\\.vcl\\'" . java-mode)
         ("\\.vm\\'" . emacs-lisp-mode)
         ("\\.wfcfg\\'" . perl-mode)
         ("\\.wsdd\\'" . nxml-mode)
         ("\\.xml$" . nxml-mode) ;; psgml-mode, nxml-mode
         ("\\.xsd$" . nxml-mode) ;; xsl-mode
         ("\\.xsl$" . nxml-mode) ;; xsl-mode
         ("\\.yaml\\'" . yaml-mode)
         ("\\config\\'" . conf-mode)
         ("control" . conf-mode)
         ("feature" . conf-mode)
         ("p4" . sh-mode)
         ("pom.xml" . nxml-mode)
         ("section-parameter" . conf-mode)
         )))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Hippe expansion
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(autoload 'hippie-exp "hippie-exp" t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Tags
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; BASH is handled by regular etags
(setq tags-table-list '(
                        "~/src/ece-scripts"
                        "~/src/my-little-friends"
                        ))

;; C style like languages like Java are handled by gtags (from GNU Global)
(autoload 'gtags-mode "gtags" "" t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Yasnippets
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(autoload 'yas/expand "yasnippet" t)
(autoload 'yas/load-directory "yasnippet" t)
(setq yas/root-directory '("~/.emacs.d/snippets"))
(mapc 'yas/load-directory yas/root-directory)
(global-set-key "\C-c\C-i" 'yas/expand)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Advanced paren mode
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;(
(autoload 'paren-activate "mic-paren" t)
(setq paren-match-face 'bold)
(setq paren-sexp-mode t)
(paren-activate)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Java
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(load "~/.emacs.d/tkj-java.el")
(load "~/.emacs.d/tkj-eclim.el")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Default browser
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq browse-url-generic-program "opera"
      browse-url-browser-function 'browse-url-generic)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; For some reason, being on different networks (as experienced in
;; Dhaka), the p4 integration made all file operation extremely slow,
;; hence the explicity loading here).
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun tkj-load-p4()
  (interactive)
  (load "$HOME/.emacs.d/tkj-p4.el")
  (p4-opened))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; XML
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; doesn't work 2008-04-14 17:51
;; (add-to-list 'flyspell-prog-text-faces 'nxml-text-face)
(setq sgml-set-face t
      sgml-xml-mode t
      ;; validiting with xmllint, so XML decl. not needed
      sgml-declaration nil
      ;; invoke xmllint for external validation
      sgml-validation-command "xmllint --noout --postvalid %s %s"
      nxhtml-skip-welcome t
      popcmp-group-alternatives nil
      ;; using additional schemas for the nxml mode
      rng-schema-locating-files
      (quote ("/usr/share/emacs/23.3/etc/schema/schemas.xml"
              "/usr/local/src/html5-el/schemas.xml"
              "~/.emacs.d/schemas.xml"))
      rng-validate-delay 3
      nxml-slash-auto-complete-flag t
      )

;; my special nXML mode settings.
(add-hook 'nxml-mode-hook
          (lambda ()
            (define-key nxml-mode-map "\C-c\C-i" 'yas/expand)
            )
          t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; HTML5 support
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(eval-after-load "rng-loc"
  '(add-to-list 'rng-schema-locating-files
                "/usr/local/src/emacs/html5-el/schemas.xml"))
;; (require 'whattf-dt)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Javascript mode
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq js2-basic-offset 2
      js2-indent-on-enter-key t
      js2-enter-indents-newline t)

;; makes j2-mode work (better) with JSON files.
(defadvice js2-reparse (before json)
	(setq js2-buffer-file-name buffer-file-name))
(ad-activate 'js2-reparse)

(defadvice js2-parse-statement (around json)
	(if (and (= tt js2-LC)
			js2-buffer-file-name
			(string-equal (substring js2-buffer-file-name -5) ".json")
			(eq (+ (save-excursion
						(goto-char (point-min))
						(back-to-indentation)
						(while (eolp)
							(next-line)
							(back-to-indentation))
						(point)) 1) js2-ts-cursor))
		(setq ad-return-value (js2-parse-assign-expr))
		ad-do-it))
(ad-activate 'js2-parse-statement)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Unfill
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun unfill-paragraph ()
  (interactive)
  (let ((fill-column (point-max)))
    (fill-paragraph nil)))

(defun unfill-region ()
  (interactive)
  (let ((fill-column (point-max)))
    (fill-region (region-beginning) (region-end) nil)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; I often want to explicitly load big Emacs modules to minimize the
;; start up time
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun tkj-load-jdibug ()
  (interactive)
  (load "~/.emacs.d/tkj-jdibug.el"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Auto insert file templates
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(load "~/.emacs.d/tkj-auto-insert.el")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Tidy
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun tidy-buffer ()
  "Run Tidy HTML parser on current buffer."
  (interactive)
  (if (get-buffer "tidy-errs") (kill-buffer "tidy-errs"))
  (shell-command-on-region (point-min) (point-max)
    "tidy -f /tmp/tidy-errs -q -wrap 72" t)
  (find-file-other-window "/tmp/tidy-errs")
  (other-window 1)
  (delete-file "/tmp/tidy-errs")
  (message "buffer tidy-ed"))

(defun tkj-tidy-up-xml()
  (interactive)
  (goto-char 0)
  (replace-string "><" ">
<")
  (indent-region (point-min) (point-max)))
(global-set-key (kbd "C-x t") 'tkj-tidy-up-xml)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Insert date
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun insert-date ()
   (interactive)
   (let (( time (current-time-string) ))
     (insert (format-time-string "%Y-%m-%d"))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Diff
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq ediff-window-setup-function 'ediff-setup-windows-plain
      ediff-diff-options "-w")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; org-mode
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(autoload 'org-export "~/.emacs.d/tkj-org.el" t)
(autoload 'org-mode "~/.emacs.d/tkj-org.el" t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; BASH
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq sh-basic-offset 2)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Various packaegs & settings to get smart file name completion
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(autoload 'file-cache-ido-find-file
  "~/.emacs.d/tkj-smart-file-name-completion.el"
  t)
(global-set-key "\C-cf" 'file-cache-ido-find-file)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Chat
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun tkj-load-chat()
  (interactive)
  (load "~/.emacs.d/tkj-chat.el")
  (load "~/.emacs.d/tkj-jabber.el"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Blockdiag
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun tkj-load-blockdiag()
  (interactive)
  (load "~/src/emacsfiles/blockdiag-mode.el"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Put all Emacs customize variables & faces in its own file
(setq custom-file "~/.emacs-custom.el")
(load custom-file 'noerror)
