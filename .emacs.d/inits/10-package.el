;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;パッケージ管理
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;package.el
;;;please execute M-x list-package at first
(require 'package)
(add-to-list 'package-archives
             '("melpa" . "http://melpa.milkbox.net/packages/") t)
(add-to-list 'package-archives
             '("marmalade" . "http://marmalade-repo.org/packages/"))
(package-initialize)

(defvar installing-package-list
  '(
    ;; ここに使っているパッケージを書く。
    cl-lib
    mode-compile
    popwin
    helm
    powerline
    magit
    ruby-mode
    ruby-end
    ido
    rinari
    rspec-mode
    direx
    highlight-indentation
    git-gutter
    auto-complete
    coffee-mode
    flymake
    flymake-coffee
    undo-tree
    markdown-mode
    smart-compile
    yasnippet
    helm-c-yasnippet
    yasnippet-bundle
    flymake-ruby
    google-translate
    motion-mode
    request
    dash-at-point
    js2-mode
    ag
    region-bindings-mode
    multiple-cursors
    rhtml-mode
    exec-path-from-shell
    guide-key
    rubocop
    auto-highlight-symbol
    smartparens
    ggtags
    inf-ruby
    robe
    ack-and-a-half
    cucumber-goto-step
    smartrep
    rainbow-delimiters
    zenburn-theme
    gitconfig-mode
    gitignore-mode
    git-commit-mode
    yaml-mode
    projectile
    popup
    ))

(let ((not-installed (loop for x in installing-package-list
                           when (not (package-installed-p x))
                           collect x)))
  (when not-installed
    (package-refresh-contents)
    (dolist (pkg not-installed)
      (package-install pkg))))
