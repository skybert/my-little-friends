;;                                                      -*- emacs-lisp -*-

(require 'org-compat)
(require 'org-list)
(require 'org-element)
(require 'org)
;; (require 'ox-reveal)

(setq org-return-follows-link t
      org-reveal-theme "blood" ;; serif
      org-reveal-root "http://skybert.net/reveal.js"
      org-agenda-files '("~/doc/scribbles/2017")
      org-capture-templates
      (quote (("t" "todo" entry
               (file "~/doc/scribbles/2017/work.org")
               "** TODO %?\n  SCHEDULED: %T\n%a\n")
              ("i" "idea" entry (file "~/doc/ideas.org")
               "** TODO %?\n  SCHEDULED: %T\n%a\n"))))

(define-key global-map "\C-ca" 'org-agenda)
(define-key global-map "\C-cl" 'org-store-link)
(define-key global-map "\C-ct" 'org-capture)

(setq remember-annotation-functions '(org-remember-annotation))
(setq remember-handler-functions '(org-remember-handler))
(add-hook 'remember-mode-hook 'org-remember-apply-template)

;; restore windows after showing the agenda
;; (add-hook 'org-agenda-after-show-hook 'winner-undo)

