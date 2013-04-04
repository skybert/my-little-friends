(require 'mu4e)

(setq mu4e-maildir "~/mail"
      mu4e-get-mail-command "offlineimap"
      
      ;; don't save messages to Sent Messages, Gmail/IMAP will take
      ;; care of this
      mu4e-sent-messages-behavior 'trash

      mu4e-view-show-images t
      mu4e-maildir-shortcuts
      '(
        ("/conduct/inbox" . ?i)
        ("/conduct/techtalk" . ?t)
        ("/conduct/vizrt-forum" . ?v)
        ("/conduct/wiki" . ?w)
        )
      )

(setq message-send-mail-function 'smtpmail-send-it
      user-mail-address "tkj@conduct.no"
      smtpmail-stream-type 'starttls
      smtpmail-default-smtp-server "smtp.gmail.com"
      smtpmail-smtp-server "smtp.gmail.com"
      smtpmail-smtp-service 587)

(defun tkj-load-mu4e-private()
  (interactive)
  (setq mu4e-maildir-shortcuts
        '(
          ("/gmail/community" . ?c)
          ("/gmail/inbox" . ?i)
          )
        user-mail-address "torstein.k.johansen@gmail.com"
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
        )
  )

;; use imagemagick, if available
(when (fboundp 'imagemagick-register-types)
  (imagemagick-register-types))
