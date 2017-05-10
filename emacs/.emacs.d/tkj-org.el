;;                                                      -*- emacs-lisp -*-

;; need the latest greatest for the exporter
(add-to-list 'load-path "/usr/local/src/emacs/org-mode/lisp")

(require 'org-compat)
(require 'org-list)
(require 'org-element)
(require 'org)
;; (require 'ox-reveal)

(setq org-return-follows-link t
      org-reveal-theme "blood" ;; serif
      org-reveal-root "http://skybert.net/reveal.js"
      org-agenda-files (list (concat "~/doc/scribbles/"
                                     (format-time-string "%Y")
                                     "/work.org")
                             "~/doc/jira.org"
                             "~/doc/someday.org"
                             "~/doc/gcal.org"
                             "~/doc/wunderlist.org"
                             )
      org-capture-templates
      (quote (("t" "todo" entry (file "~/doc/work.org")
               "** TODO %?\n  SCHEDULED: %T\n%a\n")
              ("i" "idea" entry (file "~/doc/ideas.org")
               "** TODO %?\n  SCHEDULED: %T\n%a\n")
              ("q" "QA follow up" entry (file "~/doc/work.org")
               "** TODO Follow up on QA of %?\n  SCHEDULED: %T\n%a\n"))))

(define-key global-map "\C-ca" 'org-agenda)
(define-key global-map "\C-cl" 'org-store-link)
(define-key global-map "\C-ct" 'org-capture)

(setq remember-annotation-functions '(org-remember-annotation))
(setq remember-handler-functions '(org-remember-handler))
(add-hook 'remember-mode-hook 'org-remember-apply-template)

;; restore windows after showing the agenda
;; (add-hook 'org-agenda-after-show-hook 'winner-undo)

;; don't create a footer
(setq
 org-export-html-style "<link rel=\"stylesheet\" type=\"text/css\" href=\"conduct.css\">"
 org-export-html-postamble nil
)

(defun tkj-export-to-confluence()
  (interactive)
  ;; TODO create new file with <filename>.confluence in the same
  ;; directory as the .org file
  (save-excursion
    (write-file (concat (buffer-name) ".confluence"))
    (goto-line 1)
    (query-replace "#+BEGIN_SRC js2" "{code}")
    (goto-line 1)
    (query-replace "#+BEGIN_SRC html" "{code}")
    (goto-line 1)
    (query-replace "#+BEGIN_SRC conf" "{code}")
    (goto-line 1)
    (query-replace "#+BEGIN_SRC java" "{code}")
    (goto-line 1)
    (query-replace "#+BEGIN_SRC sh" "{code}")
    (goto-line 1)
    (query-replace "#+BEGIN_SRC nxml" "{code}")
    (goto-line 1)
    (query-replace "#+BEGIN_SRC text" "{code}")
    (goto-line 1)
    (query-replace "#+END_SRC" "{code}")
    (goto-line 1)
    (query-replace "#+BEGIN_SRC" "{code}")
    (goto-line 1)
    (query-replace "****" "h4.")
    (goto-line 1)
    (query-replace "***" "h3.")
    (goto-line 1)
    (query-replace "**" "h2.")
    (goto-line 1)
    (query-replace "*" "h1.")
    (goto-line 1)
    (query-replace "
- " "
* ")
    (goto-line 1)
    (query-replace "

| " "
|| ")
    (goto-line 1)
    (query-replace "|
|-" "||
|-"))
  )

