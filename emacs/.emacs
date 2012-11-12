;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                                          ;; 
;;           Torstein Krause Johansen's .emacs file                         ;;
;;                                                                          ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setq user-full-name "Torstein Krause Johansen"
      user-mail-address "tkj@vizrt.com")

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
      "find ~/src/ece-scripts -type f | egrep -v '.(svn|git)' | xargs grep -n -i -e ")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Prefer UTF 8, but don't override current encoding if specified
;; (unless you specify a write hook).
;;
;; This seems to solve the problem of Gnus encoding the message as
;; utf-16be (64 encoded) when replying to something Bangladeshi.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(prefer-coding-system 'utf-8)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Set tabs to 2 spaces and replace all tabs with spaces
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq default-tab-width 2)
(setq-default indent-tabs-mode nil)

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Emacs package repositories
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq package-archives '(("melpa" . "http://melpa.milkbox.net/packages/")
                         ("marmalade" . "http://marmalade-repo.org/packages/")))

;; loading packages installed via the emacs24 package repositories
;; here
(let ((default-directory "~/.emacs.d/elpa"))
  (normal-top-level-add-subdirs-to-load-path))
(require 'mic-paren)
(require 'magit)

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

(global-set-key "\C-\M-g" 'jde-usages-get-signature-of-thing-at-point)
(global-unset-key "\C-x\C-c") ;; quitting too often without wanting to
(global-set-key "\C-x\C-c" 'compile) ;; imenu
;; newline and indent (like other editors, even vi, do).
(global-set-key  "\C-m" 'newline-and-indent)
(global-set-key  "\C-o" 'ecb-goto-window-methods)
;; Gnus shortcut B DELETE is way too hard ...
(global-unset-key [ (delete) ])
(global-set-key [ delete ] 'gnus-summary-delete-article)
(global-set-key  [ (f12) ] 'gnus-summary-delete-article)

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
      gnus-local-organization "Vizrt Online"
      mail-default-directory "~/mail"
      mail-from-style 'angles
      mail-interactive nil
      mail-self-blind t
      message-directory "~/mail"
)

(load "~/.emacs.d/tkj-mu4e.el")

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
;; Automatic global font-lock-mode
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(global-font-lock-mode t)

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Associate different modes with different file types.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq auto-mode-alist
      (append
       '(("\\.bashrc\\'" . sh-mode)
         ("\\.blockdiag\\'" . perl-mode)
         ("\\.bib\\'" . bibtex-mode)
         ("\\.c\\'" . c-mode) 
         ("ChangeLog" . change-log-mode)
         ("\\.cgi\\'" . python-mode) 
         ("\\.conf\\'" . conf-mode) 
         ("\\.config\\'" . conf-mode) 
         ("\\.cpp\\'" . c++-mode) 
         ("\\.css\\'" . css-mode) 
         ("\\.dtd\\'" . dtd-mode)
         ("\\.ebk\\'" . nxml-mode)
         ("\\.el\\'"  . emacs-lisp-mode)
         ("\\.emacs\\'" . emacs-lisp-mode)
         ("\\.es$" . c++-mode)
         ("\\.htm\\'" . html-mode) 
         ("\\.html\\'" . nxml-mode) 
         ("\\.jbk\\'" . nxml-mode) 
         ("\\.shtml\\'" . nxml-mode) 
         ("\\.idl\\'" . c++-mode)
         ("\\.java$" . java-mode)
         ("\\.json$" . js2-mode)
         ("\\.js$" . js2-mode)
         ("\\.jsp$" . nxml-mode) ;; nxml-mode
         ("\\.jspf$" . nxml-mode) ;; nxml-mode
         ("\\.muse$" . planner-mode)
         ("\\.odl\\'" . c++-mode) 
         ("\\.org\\'" . org-mode) 
         ("p4" . sh-mode)
         ("\\.pdf\\'" . doc-view-mode)
         ("\\.py\\'" . python-mode) 
         ("\\.php\\'" . php-mode) 
         ("\\.phtml\\'" . php-mode) 
         ("\\.pl\\'" . perl-mode)
         ("\\.properties\\'" . conf-mode)
         ("\\.properties.template\\'" . conf-mode)
         ("\\.py$" . python-mode)
         ("pom.xml" . nxml-mode)
         ("\\.sh\\'" . sh-mode) 
         ("\\.sql\\'" . sql-mode) 
         ("\\.targets$" . nxml-mode) 
         ("\\.text\\'" . text-mode)
         ("\\.tld.*\\'" . nxml-mode)
         ("\\.txt\\'" . text-mode)
         ("\\.tex\\'" . latex-mode)
         ("\\.vcl\\'" . java-mode)
         ("\\.vm\\'" . emacs-lisp-mode)
         ("\\.wfcfg\\'" . perl-mode)
         ("\\.wsdd\\'" . nxml-mode)
         ("\\.xml$" . nxml-mode) ;; psgml-mode, nxml-mode
         ("\\.xsd$" . nxml-mode) ;; xsl-mode
         ("\\.xsl$" . nxml-mode) ;; xsl-mode
         ("feature" . conf-mode)
         ("section-parameter" . conf-mode)
         )))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Hippe expansion
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'hippie-exp)

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
(require 'yasnippet)
(setq yas/root-directory '("~/.emacs.d/snippets"))
(mapc 'yas/load-directory yas/root-directory)
(global-set-key "\C-c\C-i" 'yas/expand)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Advanced paren mode
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(when (or (string-match "XEmacs\\|Lucid" emacs-version) window-system)
  (require 'mic-paren)
  (paren-activate) ; activating
  ;;; set here any of the customizable variables of mic-paren, e.g.:
  (setq paren-match-face 'bold)
  (setq paren-sexp-mode t))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; My c-mode settings, java specific settings are set in .emacs-java
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun my-c-mode-hook ()
  (setq c-basic-offset 2
        c-label-offset 0
        indent-tabs-mode nil
        compile-command "cd ~/src/p4/escenic/plugins/community/trunk/community-core; mvn test -Dtest=OpenIDProviderTest"
        require-final-newline nil)
  (auto-fill-mode)
  (c-set-offset 'substatement-open 0)
  (define-key c-mode-base-map "\C-m" 'c-context-line-break)
  (define-key c-mode-base-map "\C-c\C-i" 'yas/expand)
  (subword-mode)
  (gtags-mode)
  )
(add-hook 'c-mode-common-hook 'my-c-mode-hook)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Java
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(load "~/.emacs.d/tkj-java.el")
;; (load "~/.emacs.d/tkj-flymake.el")

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

;; HTML5 support
(eval-after-load "rng-loc"
  '(add-to-list 'rng-schema-locating-files
                "/usr/local/src/emacs/html5-el/schemas.xml"))
(require 'whattf-dt)

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
;; outline/wiki
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq outline-heading-alist
      '(("h1." . 1)
        ("h2." . 2)
        ("h3." . 3)
        ("h4." . 4)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; org-mode
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(load "~/.emacs.d/tkj-org.el")
(add-to-list 'Info-default-directory-list
             (expand-file-name "/usr/local/share/info"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; BASH
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq sh-basic-offset 2)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Various packaegs & settings to get smart file name completion
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(load "~/.emacs.d/tkj-smart-file-name-completion.el")

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(canlock-password "e4d772a3afeae6dbcbc7e1dbff60aa2ac44921b2")
 '(css-indent-offset 2)
 '(custom-safe-themes (quote ("b7553781f4a831d5af6545f7a5967eb002c8daeee688c5cbf33bf27936ec18b3" "1e7e097ec8cb1f8c3a912d7e1e0331caeed49fef6cff220be63bd2a6ba4cc365" "fc5fcb6f1f1c1bc01305694c59a1a861b008c534cae8d0e48e4d5e81ad718bc6" "54d1bcf3fcf758af4812f98eb53b5d767f897442753e1aa468cfeb221f8734f9" "d6a00ef5e53adf9b6fe417d2b4404895f26210c52bb8716971be106550cea257" "dfa78f3070e4496c444610310d095fc188d0d274" "bba5884bca1625fe327887e6b5674da2e98995b7" "9cdf9fb94f560902b567b73f65c2ed4e5cfbaafe" default)))
 '(fringe-mode 0 nil (fringe)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(diff-added ((t (:foreground "Darkgreen"))))
 '(diff-removed ((t (:foreground "Red"))))
 '(flymake-errline ((((class color)) (:underline "Red"))) t)
 '(flymake-warnline ((((class color)) (:underline "Orange"))) t))


