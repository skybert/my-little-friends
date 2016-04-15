
(require 'compile)

(setq compilation-ask-about-save nil
      compilation-scroll-output 'next-error
      ;; Don't stop on info or warnings.
      compilation-skip-threshold 2)

(provide 'tkj-compile)
