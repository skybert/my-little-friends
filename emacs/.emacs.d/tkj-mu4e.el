(setq mu4e-maildir "~/mail"
      mu4e-get-mail-command "offlineimap"
      
      ;; don't save messages to Sent Messages, Gmail/IMAP will take
      ;; care of this
      mu4e-sent-messages-behavior 'trash

      mu4e-view-show-images t
      mu4e-maildir-shortcuts
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
        )
      )

(setq message-send-mail-function 'smtpmail-send-it
      smtpmail-starttls-credentials '(("mailosl2.vizrt.internal" 587  nil nil))
      smtpmail-default-smtp-server "mailosl2.vizrt.internal"
      smtpmail-smtp-server "mailosl2.vizrt.internal"
      smtpmail-smtp-service 587
      smtpmail-local-domain "vizrt.com")

(defun tkj-load-mu4e-private()
  (interactive)
  (setq mu4e-maildir-shortcuts
      '(
        ("/gmail/community" . ?c)
        ("/gmail/inbox" . ?i)
        )
      user-mail-address "torstein.k.johansen@gmail.com"
      smtpmail-stream-type 'starttls
      smtpmail-default-smtp-server "smtp.gmail.com"
      smtpmail-smtp-server "smtp.gmail.com"
      smtpmail-smtp-service 587)
)

;; use imagemagick, if available
(when (fboundp 'imagemagick-register-types)
  (imagemagick-register-types))
