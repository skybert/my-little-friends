;;; global password setting used by jabber-auto-reconnect
(setq password-cache-expiry nil)

(setq
 jabber-auto-reconnect t
 jabber-avatar-verbose nil
 jabber-backlog-days 7
 jabber-backlog-number 100
 jabber-history-enabled t
 jabber-muc-autojoin t
 jabber-show-offline-contacts nil
 jabber-use-global-history nil
 jabber-vcard-avatars-retrieve nil
 )

(load "~/.emacs.d/tkj-jabber-private.el")
