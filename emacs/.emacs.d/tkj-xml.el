;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; XML settings
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
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
      (quote ("/usr/share/emacs/24.4/etc/schema/schemas.xml"
              "/usr/local/src/html5-el/schemas.xml"
              "~/.emacs.d/schemas.xml"))
      rng-validate-delay 3
      nxml-slash-auto-complete-flag t
      nxml-child-indent 2
      nxml-attribute-indent 2
      )

;; my special nXML mode settings.
(add-hook 'nxml-mode-hook
          (lambda ()
            (define-key nxml-mode-map "\C-c\C-i" 'yas/expand)
            (define-key nxml-mode-map ">" 'nxml-balanced-close-start-tag-block)
            )
          t)

;; Showing the curren xpath in the echo area.
;; taken from http://www.emacswiki.org/emacs/NxmlMode
(defun nxml-where ()
  "Display the hierarchy of XML elements the point is on as a path."
  (interactive)
  (let ((path nil))
    (save-excursion
      (save-restriction
        (widen)
        ;; Doesn't error if point is at beginning of buffer
        (while (and (< (point-min) (point))
                    (condition-case nil
                        (progn
                          (nxml-backward-up-element) ; always returns nil
                          t)
                      (error nil)))
          (setq path (cons (xmltok-start-tag-local-name) path)))
        (if (called-interactively-p t)
            (message (concat "You are here: "
             (mapconcat 'identity path " > "))))))))

(defun tkj-html-entities-to-char()
  (interactive)
  (save-excursion
    (goto-char 0)
    (while (search-forward "&amp;" nil t) (replace-match "&" nil t))
    (goto-char 0)
    (while (search-forward "&lt;" nil t) (replace-match "<" nil t))
    (goto-char 0)
    (while (search-forward "&gt;" nil t) (replace-match ">" nil t))
    (goto-char 0)
    (while (search-forward "&oslash;" nil t) (replace-match "ø" nil t))
    (goto-char 0)
    (while (search-forward "&aring;" nil t) (replace-match "å" nil t))
    (goto-char 0)
    (while (search-forward "&aelig;" nil t) (replace-match "æ" nil t))
    ))

(defun tkj-tidy-up-xml()
  (interactive)
  (goto-char 0)
  (replace-string "><" ">
<")
  (indent-region (point-min) (point-max)))
(global-set-key (kbd "C-x t") 'tkj-tidy-up-xml)

