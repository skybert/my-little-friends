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
      (quote ("/usr/share/emacs/24.3/etc/schema/schemas.xml"
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
            (define-key nxml-mode-map "\M- " 'nxml-complete)
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
            (setq frame-title-format (concat invocation-name "@"
                                             (system-name)
                                             " {%f}"
                                             " xpath: /"
                                             (mapconcat 'identity path "/")))
          (format "/%s" (mapconcat 'identity path "/")))))))

(defun tkj-html-entities-to-char()
  (interactive)
  (save-excursion
    (goto-char 0)
    (replace-string "&amp;" "&")
    (goto-char 0)
    (replace-string "&lt;" "<")
    (goto-char 0)
    (replace-string "&gt;" ">")
    (goto-char 0)
    (replace-string "&oslash;" "ø")
    (goto-char 0)
    (replace-string "&aring;" "å")
    (goto-char 0)
    (replace-string "&aelig;" "æ")
    ))
