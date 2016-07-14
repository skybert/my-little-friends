;; VC related changes, also see tkj-p4.el for p4 specific changes

(add-hook 'git-commit-mode-hook 'ac-emoji-setup)

;; Remove the 'Git-' prefix from the modeline when displaying the
;; current branch.
(setcdr (assq 'vc-mode mode-line-format)
        '((:eval (replace-regexp-in-string "^ Git-" " " vc-mode))))
