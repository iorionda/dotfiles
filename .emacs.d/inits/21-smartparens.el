(require 'smartparens-config)

(package-initialize)
(smartparens-global-mode t)

(show-smartparens-global-mode +1)

;; smartparens for ruby
(eval-after-load "ruby-mode"
  '(progn
     (require 'smartparens-ruby)
     (set-face-attribute 'sp-show-pair-match-face nil
                         :background "grey20" :foreground "green"
                         :weight 'semi-bold)))

(add-hook 'ruby-mode-hook 'show-smartparens-mode)
