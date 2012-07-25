(setq mu4e-maildir "~/mail"
      mu4e-get-mail-command "offlineimap"
      
      ;; don't save messages to Sent Messages, Gmail/IMAP will take
      ;; care of this
      mu4e-sent-messages-behavior 'trash
      )

(setq message-send-mail-function 'smtpmail-send-it
      smtpmail-stream-type 'starttls
      smtpmail-default-smtp-server "smtp.gmail.com"
      smtpmail-smtp-server "smtp.gmail.com"
      smtpmail-smtp-service 587)
