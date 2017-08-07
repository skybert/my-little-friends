(setq ispell-program-name "aspell"
      ispell-list-command "list"
      ispell-dictionary "british"
      flyspell-auto-correct-binding (kbd "<S-f12>"))

(when (executable-find "hunspell")
  (setq-default ispell-program-name "hunspell")
  (setq ispell-really-hunspell t))
