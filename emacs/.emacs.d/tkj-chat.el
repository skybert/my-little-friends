;; ERC modules
(require 'erc-log)
(require 'erc-autoaway)
(require 'erc-image)
(require 'erc-colorize)
(require 'erc-tweet)

(setq erc-default-server "localhost"
      erc-log-channels-directory "~/.erc/logs"
      erc-log-write-after-send t
      erc-autoaway-idle-seconds 600
      erc-enable-logging t
      erc-save-buffer-on-part t
      erc-track-exclude-types '("JOIN" "PART" "QUIT" "NICK" "MODE")
      erc-hide-list '("JOIN" "PART" "QUIT" "MODE"))

;; M-RET opens link at point
(define-key erc-mode-map (kbd "M-<return>") 'browse-url)

(erc-log-mode)

;; general options
(setq erc-default-server "localhost")

;; Adjust text wrapping/filling whenever the window is resized
(add-hook 'window-configuration-change-hook
          '(lambda ()
             (setq erc-fill-column (- (window-width) 2))))

;; Spell checking
(add-hook 'erc-mode-hook 'flyspell-mode)

;; emojies
(company-emoji-init)
(add-hook 'erc-mode-hook 'emojify-mode)

;; From the bitlbee wiki: Since the server sends wrong JIDs for the
;; "from" field (123456_chat_name@conf.hipchat.com/real name here),
;; all you can do is using client scripts to fix this up
(defun my-reformat-jabber-backlog ()
  (save-excursion
    (goto-char (point-min))
    (if (looking-at
         "^<root> Message from unknown participant \\([^:]+\\):")
        (replace-match "<\\1>"))))
(add-hook 'erc-insert-modify-hook 'my-reformat-jabber-backlog)

(add-to-list 'erc-modules 'tweet)
(add-to-list 'erc-modules 'image)
(erc-update-modules)

(defun tkj-close-some-chats()
  (interactive)
  (condition-case nil (kill-buffer "#cloud") (error nil))
  (condition-case nil (kill-buffer "#ng") (error nil))
  (condition-case nil (kill-buffer "#support-operations") (error nil))
  (condition-case nil (kill-buffer "#platform-dhaka") (error nil))
  (condition-case nil (kill-buffer "#cue-notifications") (error nil))
  (condition-case nil (kill-buffer "#ece-notificiations") (error nil))
  (condition-case nil (kill-buffer "#qa") (error nil)))

;; emojies
(setq emojify-display-style 'image)

;; Slack settings, slack-register-team and slack-start called from
;; ~/.emacs.d/custom.el which is NOT checked in 😉
(require 'slack)
(setq slack-enable-emoji t
      ;; don't like the icon set used
      slack-buffer-emojify t
      slack-typing-visibility 'buffer
      slack-prefer-current-team t
      lui-prompt-string "陶亭> "
      )

(add-hook 'slack-mode-hook 'flyspell-mode)

(define-key slack-mode-map (kbd "<f5>") 'slack-room-update-messages)
(define-key slack-mode-map (kbd "C-c C-e") 'slack-message-edit)
(define-key slack-mode-map (kbd "C-c C-r") 'slack-message-add-reaction)
(define-key slack-mode-map (kbd "C-c C-s") 'slack-message-show-reaction-users)

(alert-define-style
 'tkj-slack-alert-style :title "tkj notifier Style"
 :notifier
 (lambda (info)
   ;; The message text is :message
   (plist-get info :message)
   ;; The :title of the alert
   (plist-get info :title)
   ;; The :category of the alert
   (plist-get info :category)
   ;; The major-mode this alert relates to
   (plist-get info :mode)
   ;; The buffer the alert relates to
   (plist-get info :buffer)
   ;; Severity of the alert.  It is one of:
   ;;   `urgent'
   ;;   `high'
   ;;   `moderate'
   ;;   `normal'
   ;;   `low'
   ;;   `trivial'
   (plist-get info :severity)
   ;; Whether this alert should persist, or fade away
   (plist-get info :persistent)
   ;; Data which was passed to `alert'.  Can be
   ;; anything.
   (plist-get info :data)

   (save-excursion
     (set-buffer (get-buffer-create "*slack-messages*"))
     (goto-char (point-max))
     (insert
      (concat
       (format-time-string "%Y-%m-%d %H:%M:%S")
       " 📺 " (plist-get info :title)
       (if (plist-get info :message)
           (concat " 📰 " (plist-get info :message)))
       "\n"))
     (write-region nil nil "~/.slack-messages.log")
     )

   ;; Removers are optional.  Their job is to remove
   ;; the visual or auditory effect of the alert.
   :remover
   (lambda (info)
     ;; It is the same property list that was passed to
     ;; the notifier function.
     )))

(setq alert-default-style 'tkj-slack-alert-style)
;; (alert "hello" :title "the title")
