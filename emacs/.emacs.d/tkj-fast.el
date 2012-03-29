;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                                          ;; 
;;           Torstein Krause Johansen's .emacs file,                        ;;
;;           Minimum settings needed in any Emacs session.                  ;;
;;                                                                          ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq user-full-name "Torstein Krause Johansen"
      user-mail-address "tkj@vizrt.com")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Appearance
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(menu-bar-mode 0)

;; (set-scroll-bar-mode nil)
;; (tool-bar-mode 0)
;; (fringe-mode 0)

;; (set-background-color "black")
;; (set-foreground-color "#aad6b1")
;; (set-cursor-color "red")

(setq frame-background-mode nil
      column-number-mode t
      frame-title-format (concat invocation-name "@" (system-name) " {%f}")
      ;; no visible or audible bells, please
      visible-bell nil
      ring-bell-function (lambda nil (message ""))
      show-paren-mode t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Shortcuts in all modes
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(global-set-key "\C-xg" 'goto-line)
(global-set-key "\M- " 'hippie-expand)
(global-set-key "\C-\M-f" 'find-file-at-point)
(global-set-key "\C-cn" 'find-dired)
(global-set-key "\C-cN" 'grep-find)
;; don't write backslashed to indicate continuous lines
(set-display-table-slot standard-display-table 'wrap ?\ )
;; Treat 'y' or <CR> as yes, 'n' as no.
(fset 'yes-or-no-p 'y-or-n-p)
(setq grep-find-command
      "find ~/src/ece-scripts/usr/{bin,sbin,share} -name \"*\" -print0 | xargs -0 -e grep -n -i -e ")

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

