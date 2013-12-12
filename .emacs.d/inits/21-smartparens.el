;; (require 'smartparens-config)
(require 'smartparens)

;;;;;;;;;;;;;;;;;;
;; global
(package-initialize)
(smartparens-global-mode t)

;;;;;;;;;;;;;;;;;;
;; highlights matching pairs
(show-smartparens-global-mode t)

;;;;;;;;;;;;;;;;;;
;; pair management
(sp-local-pair 'minibuffer-inactive-mode "'" nil :actions nil)

;;;;;;;;;;;;;;;;;;
;; ruby-mode
(eval-after-load "ruby-mode"
  '(progn
     (require 'smartparens-ruby)
     (set-face-attribute 'sp-show-pair-match-face nil
                         :background "grey20" :foreground "green"
                         :weight 'semi-bold)))

(add-hook 'ruby-mode-hook 'show-smartparens-mode)
