(require 'mu4e)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Name and email
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq user-full-name "Torstein Krause Johansen"
      user-mail-address "torstein@escenic.com")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Mail (and news), common to both Gnus and VM
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq gnus-agent-directory "~/mail/agent"
      gnus-article-save-directory "~/mail"
      gnus-cache-directory "~/mail/cache"
      gnus-directory "~/mail"
      gnus-dribble-directory "~/mail/dribble"
      gnus-local-organization "Escenic"
      mail-default-directory "~/mail"
      mail-from-style 'angles
      mail-interactive nil
      mail-self-blind t
      message-directory "~/mail"
      )

(setq mu4e-maildir "~/mail"
      mu4e-attachment-dir  "~/tmp"
      mu4e-get-mail-command "offlineimap"
      mu4e-debug nil
      mu4e-use-fancy-chars t
      ;; don't save messages to Sent Messages, Gmail/IMAP will take
      ;; care of this
      mu4e-sent-messages-behavior 'trash

      mu4e-view-show-images nil

      ;; Easier to read HTML email in dark themes
      shr-color-visible-luminance-min 80

      mu4e-compose-signature t

      ;; See C-h v mu4e-header-info for more
      mu4e-headers-fields
      '( (:date          .  12)
         (:maildir       .  20)
         (:from          .  22)
         (:subject       .  nil))

      ;; common SMTP settings for all accounts
      message-send-mail-function 'smtpmail-send-it

      ;; Make GPG work with emacs daemon (by default, Emacs will
      ;; attempt and fail to connect to the GPG agent and then fail
      ;; the GPG alltogether.
      epa-pinentry-mode 'loopback)

;; Navigate links in rich text email by Tab/Shift + Tab
(add-hook 'mu4e-view-mode-hook
  (lambda()
    (local-set-key (kbd "<tab>") 'shr-next-link)
    (local-set-key (kbd "<backtab>") 'shr-previous-link)))

(defun tkj-load-mu4e-escenic()
  (interactive)
  (require 'org-mu4e)
  (setq  mu4e-maildir-shortcuts
         '(
           ("/escenic/inbox" . ?i)
           ("/escenic/jira" . ?j)
           ("/escenic/build" . ?e)
           ("/escenic/wiki" . ?w)
           )
         user-mail-address "torstein@escenic.com"
         smtpmail-smtp-server "smtp.gmail.com"
         smtpmail-smtp-service 587
         smtpmail-stream-type 'starttls
         message-signature-file "~/.signature-escenic"
         )
  )

(defun tkj-load-mu4e-gmail()
  (interactive)
  (setq mu4e-maildir-shortcuts
        '(
          ("/gmail/community" . ?c)
          ("/gmail/inbox" . ?i)
          )
        user-mail-address "torstein.k.johansen@gmail.com"
        smtpmail-smtp-server "smtp.gmail.com.personal"
        smtpmail-smtp-service 587
        smtpmail-stream-type 'starttls
        message-signature-file "~/.signature-gmail"
        )
  )

(defun tkj-load-mu4e-gmailw()
  (interactive)
  (setq mu4e-maildir-shortcuts
        '(
          ("/gmailw/saas-alerts" . ?a)
          ("/gmailw/community" . ?c)
          ("/gmailw/developers-list" . ?d)
          ("/gmailw/engine-5-list" . ?e)
          ("/gmailw/vizrt-forum" . ?f)
          ("/gmailw/inbox" . ?i)
          ("/gmailw/jira" . ?j)
          ("/gmailw/p4" . ?p)
          ("/gmailw/saas" . ?s)
          ("/gmailw/twitter" . ?t)
          )
        user-mail-address "torsteinkrausework@gmail.com"
        smtpmail-smtp-server "smtp.gmail.com.work"
        smtpmail-smtp-service 587
        smtpmail-stream-type 'starttls
        message-signature-file "~/.signature-escenic"
        )
  )

(defun tkj-highlight-message-hook()
  "Highlights messages, especially diffs."
  (interactive)
  (highlight-lines-matching-regexp "^ \\+.*" 'hi-green-b)
  (highlight-lines-matching-regexp "^ \\-.*" 'hi-red-b))
(add-hook 'mu4e-view-mode-hook 'tkj-highlight-message-hook)

;; quickly change account
(define-key mu4e-main-mode-map (kbd "<f1>") 'tkj-load-mu4e-gmail)
(define-key mu4e-main-mode-map (kbd "<f2>") 'tkj-load-mu4e-gmailw)
(define-key mu4e-main-mode-map (kbd "<f4>") 'tkj-load-mu4e-escenic)
(define-key mu4e-headers-mode-map (kbd "<f1>") 'tkj-load-mu4e-gmail)
(define-key mu4e-headers-mode-map (kbd "<f2>") 'tkj-load-mu4e-gmailw)
(define-key mu4e-headers-mode-map (kbd "<f4>") 'tkj-load-mu4e-escenic)

;; use imagemagick, if available
(when (fboundp 'imagemagick-register-types)
  (imagemagick-register-types))

;; default profile
(tkj-load-mu4e-gmail)
