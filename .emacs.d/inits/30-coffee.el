;;;coffee-mode
(require 'coffee-mode)

(setq whitespace-action '(auto-cleanup)) ;; automatically clean up bad whitespace
(setq whitespace-style '(trailing space-before-tab indentation empty space-after-tab)) ;; only show bad whitespace

(defun coffee-custom ()
  "coffee-mode-hook"
  (set (make-local-variable 'tab-width) 2)
  (set (make-local-variable 'coffee-tab-width) 2))

(add-hook 'coffee-mode-hook
          '(lambda() (coffee-custom)))
