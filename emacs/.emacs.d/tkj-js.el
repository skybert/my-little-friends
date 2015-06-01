;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Javascript mode
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq js2-basic-offset 2
      js2-indent-on-enter-key t
      js2-enter-indents-newline t)

;; makes j2-mode work (better) with JSON files.
(defadvice js2-reparse (before json)
  (setq js2-buffer-file-name buffer-file-name))
(ad-activate 'js2-reparse)

(defadvice js2-parse-statement (around json)
  (if (and (= tt js2-LC)
      js2-buffer-file-name
      (string-equal (substring js2-buffer-file-name -5) ".json")
      (eq (+ (save-excursion
            (goto-char (point-min))
            (back-to-indentation)
            (while (eolp)
              (next-line)
              (back-to-indentation))
            (point)) 1) js2-ts-cursor))
    (setq ad-return-value (js2-parse-assign-expr))
    ad-do-it))
(ad-activate 'js2-parse-statement)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Typescript mode
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'typescript)
(add-to-list 'auto-mode-alist '("\\.ts\\'" . typescript-mode))
(require 'tss)

(setq tss-popup-help-key "C-:"
      tss-jump-to-definition-key "C->"
      tss-implement-definition-key "C-c j"
      typescript-indent-level 2)

(tss-config-default)
