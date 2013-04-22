(require 'mu4e)

(setq mu4e-maildir "~/mail"
      mu4e-get-mail-command "offlineimap"

      ;; don't save messages to Sent Messages, Gmail/IMAP will take
      ;; care of this
      mu4e-sent-messages-behavior 'trash

      mu4e-view-show-images t

      ;; common SMTP settings for all accounts
      message-send-mail-function 'smtpmail-send-it
      smtpmail-smtp-service 587
      smtpmail-stream-type 'starttls
      )

(defun tkj-load-mu4e-conduct()
  (interactive)
  (setq  mu4e-maildir-shortcuts
         '(
           ("/conduct/inbox" . ?i)
           ("/conduct/techtalk" . ?t)
           ("/conduct/vizrt-forum" . ?v)
           ("/conduct/wiki" . ?w)
           )
         user-mail-address "tkj@conduct.no"
         smtpmail-smtp-server "smtp.gmail.com.conduct"
         message-signature-file "~/.signature-conduct"
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
        message-signature-file "~/.signature-conduct"
        )
  )

;; quickly change account
(define-key mu4e-main-mode-map (kbd "<f1>") 'tkj-load-mu4e-gmail)
(define-key mu4e-main-mode-map (kbd "<f2>") 'tkj-load-mu4e-gmailw)
(define-key mu4e-main-mode-map (kbd "<f4>") 'tkj-load-mu4e-conduct)
(define-key mu4e-headers-mode-map (kbd "<f1>") 'tkj-load-mu4e-gmail)
(define-key mu4e-headers-mode-map (kbd "<f2>") 'tkj-load-mu4e-gmailw)
(define-key mu4e-headers-mode-map (kbd "<f4>") 'tkj-load-mu4e-conduct)

;; use imagemagick, if available
(when (fboundp 'imagemagick-register-types)
  (imagemagick-register-types))

;; default profile
(tkj-load-mu4e-conduct)
