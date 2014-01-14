(unless (package-installed-p 'inf-ruby)
  (package-install 'inf-ruby))

(autoload 'inf-ruby "inf-ruby" "Run an inferior Ruby process" t)
(add-hook 'ruby-mode-hook 'inf-ruby-minor-mode)

(add-hook 'after-init-hook 'inf-ruby-switch-setup)
