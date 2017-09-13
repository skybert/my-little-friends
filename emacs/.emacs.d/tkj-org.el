;;                                                      -*- emacs-lisp -*-

(require 'org-compat)
(require 'org-list)
(require 'org-element)
(require 'org)

(setq org-return-follows-link t
      org-reveal-theme "blood" ;; serif
      org-reveal-root "http://skybert.net/reveal.js"
      org-agenda-files '("~/doc/scribbles/2017")
      org-blank-before-new-entry '((heading . always) (plain-list-item . auto))
      org-capture-templates
      (quote (("t" "todo" entry (file "~/doc/scribbles/2017/work.org")
               "** TODO %?\n  SCHEDULED: %T\n%a\n")
              ("f" "follow-up" entry (file "~/doc/scribbles/2017/work.org")
               "** TODO Follow up on %? :noreport:\n  SCHEDULED: %T\n%a\n")
              ("e" "ece" entry (file "~/doc/scribbles/2017/ece.org")
               "** TODO %?\n  SCHEDULED: %T\n%a\n")
              ("c" "cloud" entry (file "~/doc/scribbles/2017/cloud.org")
               "** TODO %?\n  SCHEDULED: %T\n%a\n")
              ("a" "adm" entry (file "~/doc/scribbles/2017/adm.org")
               "** TODO %?\n  SCHEDULED: %T\n%a\n")
              ("i" "idea" entry (file "~/doc/ideas.org")
               "** TODO %?\n  SCHEDULED: %T\n%a\n"))))

(define-key global-map "\C-ca" 'org-agenda)
(define-key global-map "\C-cl" 'org-store-link)
(define-key global-map "\C-ct" 'org-capture)

(setq remember-annotation-functions '(org-remember-annotation))
(setq remember-handler-functions '(org-remember-handler))
(add-hook 'remember-mode-hook 'org-remember-apply-template)
