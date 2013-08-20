;; -*- Mode: Emacs-Lisp ; Coding: utf-8 -*-
(require 'cl)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ~/.emacs.d/site-lisp 以下全部読み込み
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(let ((default-directory (expand-file-name "~/.emacs.d/site-lisp")))
    (add-to-list 'load-path default-directory)
      (if (fboundp 'normal-top-level-add-subdirs-to-load-path)
                (normal-top-level-add-subdirs-to-load-path)))

(require 'init-loader)
(setq init-loader-show-log-after-init nil)
(init-loader-load "~/.emacs.d/inits")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;load-path
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq load-path (append
                 '("~/.emacs.d"
                   "~/.emacs.d/color-theme"
                   "~/.emacs.d/packages"
                   "~/.emacs.d/plugins"
                   "~/.emacs.d/elisp")
                 load-path))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;日本語設定
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Localeに合わせた環境の設定
(set-locale-environment nil)
(set-language-environment 'Japanese)

(prefer-coding-system 'utf-8)
(setq default-coding-systems 'utf-8)
(setq file-name-coding-system 'utf-8)
(setq locale-coding-system 'utf-8)

;; "yes or no" を "y or n" に
(fset 'yes-or-no-p 'y-or-n-p)

;; 再帰的にgrep
(require 'grep)
(setq grep-command-before-query "grep -nH -r -e ")
(defun grep-default-command ()
  (if current-prefix-arg
      (let ((grep-command-before-target
             (concat grep-command-before-query
                     (shell-quote-argument (grep-tag-default)))))
        (cons (if buffer-file-name
                  (concat grep-command-before-target
                          " *."
                          (file-name-extension buffer-file-name))
                (concat grep-command-before-target " ."))
              (+ (length grep-command-before-target) 1)))
    (car grep-command)))
(setq grep-command (cons (concat grep-command-before-query " .")
                         (+ (length grep-command-before-query) 1)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;外観
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; スプラッシュを表示しない
(setq inhibit-startup-screen t)

;; 保存時にTAB
;; オートインデントでTABを使う
(setq-default indent-tabs-mode nil)
;;デフォルトのTAB幅を4に
(setq-default tab-width 4)
(setq tab-stop-list '(4 8 12 16 20 24 28 32 36 40 44 48 52 56 60
                      64 68 72 76 80 84 88 92 96 100 104 108 112 116 120))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 基本
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 行
;;; 行の先頭でC-kを一回押すだけで行全体を消去する
(setq kill-whole-line t)
;;; 最終行に必ず一行挿入する
(setq require-final-newline t)
;;; バッファの最後でnewlineで新規行を追加するのを禁止する
(setq next-line-add-newlines nil)

;;通常のウィンドウ用の設定
(setq-default truncate-lines t)
;;ウィンドウを左右に分割したとき用の設定
(setq-default truncate-partial-width-windows t)

;; 保存時に行末の空白を削除する
(add-hook 'before-save-hook 'delete-trailing-whitespace)

;;補完
;;; 補完時に大文字小文字を区別しない
(setq completion-ignore-case t)
(setq read-file-name-completion-ignore-case t)

;;; 補完可能なものを随時表示
(icomplete-mode 1)

;;履歴
;;; 履歴数を大きくする
(setq history-length 10000)
;;; ミニバッファの履歴を保存する
(savehist-mode 1)
;;; 最近開いたファイルを保存する数を増やす
(setq recentf-max-saved-items 10000)

;;実行権限を付与
(add-hook 'after-save-hook
          'executable-make-buffer-file-executable-if-script-p)

;;保存時に git-now を実行する
;; (when (executable-find "git-now")
;;   (defun git-now-after-save-hook()
;;     (shell-command "git now")))

;; (add-hook 'after-save-hook
;;           'git-now-after-save-hook)

;;関数名を表示する
(which-function-mode 1)

;;; 画像ファイルを表示
(auto-image-file-mode t)

;; トラックパッド用のスクロール設定
(defun scroll-down-with-lines ()
  "" (interactive) (scroll-down 3))
(defun scroll-up-with-lines ()
  "" (interactive) (scroll-up 3))
(global-set-key [wheel-up] 'scroll-down-with-lines)
(global-set-key [wheel-down] 'scroll-up-with-lines)
(global-set-key [double-wheel-up] 'scroll-down-with-lines)
(global-set-key [double-wheel-down] 'scroll-up-with-lines)
(global-set-key [triple-wheel-up] 'scroll-down-with-lines)
(global-set-key [triple-wheel-down] 'scroll-up-with-lines)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 環境
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ruby
(setenv "PATH" (concat (getenv "HOME") "/usr/local/var/rbenv/shims:"
                       (getenv "HOME") "/usr/local/var/rbenv/bin:" (getenv "PATH")))
(setq exec-path (cons (concat (getenv "HOME") "/usr/local/var/rbenv/shims")
                      (cons (concat (getenv "HOME") "/usr/local/var/rbenv/bin") exec-path)))

(dolist (dir (list
              "/usr/local/var/rbenv/shims/"
              ))
  (when (and (file-exists-p dir) (not (member dir exec-path)))
    (setenv "PATH" (concat dir ":" (getenv "PATH")))
    (setq exec-path (append (list dir) exec-path))))

(setq shell-file-name "/usr/local/bin/zsh")
(setq explicit-shell-file-name "/usr/local/bin/zsh")

;; google-translate
(require 'google-translate)
(global-set-key (kbd "C-x t") 'google-translate-at-point)
;; 翻訳のデフォルト値を設定(en -> ja)
(custom-set-variables
 '(google-translate-default-source-language "en")
 '(google-translate-default-target-language "ja"))

;;;popwin.el :pop up window for emacs baffer
(require 'popwin)
(setq display-buffer-function 'popwin:display-buffer)
(setq popwin:popup-window-height 0.5)
(defvar popwin:special-display-config-backup popwin:special-display-config)
(setq popwin:special-display-config
      (append '(("*Remember*" :stick t)("*Org Agenda*")("*Backtrace*")
                ("*sdic*" :noselect))
              popwin:special-display-config))
(define-key global-map (kbd "C-x p") 'popwin:display-last-buffer)
(push '("*Google Translate*") popwin:special-display-config)

;;;helm.el
(require 'helm)
(require 'helm-config)
(helm-mode 1)

(defun my-helm ()
  (interactive)
  (helm :sources '(
                   helm-c-source-buffers-list
                   helm-c-source-recentf
                   helm-c-source-files-in-current-dir
                   helm-c-source-mac-spotlight
                   helm-c-source-buffer-not-found)
        :buffer "*my helm*"))

(global-set-key (kbd "C-x b") 'my-helm)
(global-set-key (kbd "M-x") 'helm-M-x)
(global-set-key (kbd "C-x C-r") 'helm-recentf)
(global-set-key (kbd "M-y") 'helm-show-kill-ring)

(setq helm-samewindow nil)
(push '("*helm-M-x*") popwin:special-display-config)

;; emacsの終了時に、履歴を保存する
(remove-hook 'kill-emacs-hook 'helm-c-adaptive-save-history)
;; ディレイは0.2秒
(setq helm-input-idle-delay 0.02)
;; 候補のディレクトリが一つしかない場合に、自動的に展開しない
(setq helm-ff-auto-update-initial-value nil)

;;;magit
(require 'magit)
;; 色変更
(set-face-foreground 'magit-diff-add "#b9ca4a") ; 追加した部分を緑に
(set-face-foreground 'magit-diff-del "#d54e53")  ; 削除した 部分を赤に
(set-face-background 'magit-item-highlight "#000000") ; 選択項目ハイライトがうっとうしいので背景色と同化

;;;ruby-mode
(autoload 'ruby-mode "ruby-mode"
    "Mode for editing ruby source files" t)
(add-to-list 'auto-mode-alist '("Rakefile$" . ruby-mode))
(add-to-list 'auto-mode-alist '("\\.rb$" . ruby-mode))
(add-to-list 'auto-mode-alist '("\\.rake$" . ruby-mode))
(add-to-list 'auto-mode-alist '("Capfile$" . ruby-mode))
(add-to-list 'auto-mode-alist '("Gemfile$" . ruby-mode))

;;;ruby-end
(require 'ruby-end)
(add-hook 'ruby-mode-hook
  '(lambda ()
     (setq tab-width 2)
     (setq ruby-indent-level tab-width)
     (setq ruby-deep-indent-paren-style nil)
     (define-key ruby-mode-map [return] 'ruby-reindent-then-newline-and-indent)
     (abbrev-mode 1)
     (electric-pair-mode t)
     (electric-indent-mode t)
     (electric-layout-mode t)))

;;;ido
(require 'ido)
(ido-mode t)

;;;rinari
(require 'rinari)

;;;rspec-mode
(require 'rspec-mode)

;;;direx
(require 'direx)
(require 'direx-project)
(setq direx:leaf-icon "  "
      direx:open-icon "▾"
      direx:closed-icon "▸")
(push '(direx:direx-mode
        :position left
        :width 20
        :dedicated t)
      popwin:special-display-config)
(global-set-key
 (kbd "C-x C-j")
 'direx-project:jump-to-project-root-other-window)

;;;highlight-indentation
(require 'highlight-indentation)
(setq highlight-indentation-offset 2)
(set-face-background 'highlight-indentation-face "#696969")
(set-face-background 'highlight-indentation-current-column-face "#c3b3b3")
(add-hook 'ruby-mode-hook 'highlight-indentation-mode)
(highlight-indentation-mode)

;;;git-gutter
(require 'git-gutter)
(global-git-gutter-mode t)
(define-key global-map (kbd "C-x g") 'git-gutter:toggle)
(setq git-gutter:window-width 2)

(setq git-gutter:modified-sign " ")
(setq git-gutter:added-sign "+")
(setq git-gutter:deleted-sign "-")

(set-face-foreground 'git-gutter:modified "blue")
(set-face-foreground 'git-gutter:added "green")
(set-face-foreground 'git-gutter:deleted "red")

;;;auto-complete
(require 'auto-complete)
(global-auto-complete-mode t)
(add-to-list 'ac-dictionary-directories "~/.emacs.d/elpa/auto-complete-20130209.651/dict")
(require 'auto-complete-config)
(ac-config-default)

;; flymake-coffee
(require 'flymake-coffee)

;;;undo-tree
(require 'undo-tree)
(global-undo-tree-mode t)
(global-set-key (kbd "M-/") 'undo-tree-redo)

;; tern
(autoload 'tern-mode "tern.el" nil t)
(add-hook 'js-mode-hook (lambda () (tern-mode t)))

;;; markdown-mode
(require 'markdown-mode)

;; smart-compile
(require 'smart-compile)
(define-key ruby-mode-map (kbd "C-c c") 'smart-compile)
(define-key ruby-mode-map (kbd "C-c C-c") (kbd "C-c c C-m"))

;; rbenv でインストールした ruby を smart-compile で使う
(setenv "PATH" (concat (expand-file-name "/usr/local/opt/rbenv/shims:") (getenv "PATH")))

;;; yasnippet
(require 'yasnippet)

;; helm-c-yasnippet
(require 'helm-c-yasnippet)

;; yasnippet-bundle
(require 'yasnippet-bundle)

(yas/initialize)
(setq yas-snippet-dirs
      '("~/.emacs.d/snippets"))
(yas/global-mode 1)

;; 単語展開キーバインド (ver8.0から明記しないと機能しない)
;; (setqだとtermなどで干渉問題ありでした)
;; もちろんTAB以外でもOK 例えば "C-;"とか
(custom-set-variables '(yas-trigger-key "TAB"))
;; 既存スニペットを挿入する
(define-key yas/minor-mode-map (kbd "C-x i i") 'yas/insert-snippet)
;; 新規スニペットを作成するバッファを用意する
(define-key yas/minor-mode-map (kbd "C-x i n") 'yas/new-snippet)
;; 既存スニペットを閲覧・編集する
(define-key yas/minor-mode-map (kbd "C-x i v") 'yas/visit-snippet-file)

;; flymake
(when (locate-library "flymake")
  (require 'flymake)

  ;;シンタックスチェックは次のコマンドが呼ばれる
  ;;make -s -C . CHK_SOURCES=hoge.cpp SYNTAX_CHECK_MODE=1 check-syntax
  ;;
  ;; Makefile があれば、次のルールを追加
  ;;PHONY: check-syntax
  ;;#check-syntax:
  ;;#$(CC) -Wall -Wextra -pedantic -fsyntax-only $(CHK_SOURCES)
  ;;
  ;;CHECKSYNTAX.c = $(CC) $(CFLAGS) $(CPPFLAGS) -Wall -Wextra -pedantic -fsyntax-only
  ;;CHECKSYNTAX.cc = $(CXX) $(CXXFLAGS) $(CPPFLAGS) -Wall -Wextra -pedantic -fsyntax-only
  ;;
  ;;check-syntax: $(addsuffix -check-syntax,$(CHK_SOURCES))
  ;;%.c-check-syntax:  ; $(CHECKSYNTAX.c)  $*.c
  ;;%.cc-check-syntax: ; $(CHECKSYNTAX.cc) $*.cc


  ;; GUIの警告は表示しない
  (setq flymake-gui-warnings-enabled nil)

  ;; 全てのファイルで flymakeを有効化
  (add-hook 'find-file-hook 'flymake-find-file-hook)

  ;; flymake を使えない場合をチェック
  (defadvice flymake-can-syntax-check-file
    (after my-flymake-can-syntax-check-file activate)
    (cond
     ((not ad-return-value))
     ;; tramp 経由であれば、無効
     ((and (fboundp 'tramp-list-remote-buffers)
           (memq (current-buffer) (tramp-list-remote-buffers)))
      (setq ad-return-value nil))
     ;; 書き込み不可ならば、flymakeは無効
     ((not (file-writable-p buffer-file-name))
      (setq ad-return-value nil))
     ;; flymake で使われるコマンドが無ければ無効
     ((let ((cmd (nth 0 (prog1
                            (funcall (flymake-get-init-function buffer-file-name))
                          (funcall (flymake-get-cleanup-function buffer-file-name))))))
        (and cmd (not (executable-find cmd))))
      (setq ad-return-value nil))
     ))

  ;; M-p/M-n で警告/エラー行の移動
  (global-set-key "\M-p" 'flymake-goto-prev-error)
  (global-set-key "\M-n" 'flymake-goto-next-error)

  ;; 警告エラー行の表示
  (global-set-key "\C-cd"
                  '(lambda ()
                     (interactive)
                     (my-flymake-display-err-minibuf-for-current-line)
                     ;; (my-flymake-display-err-popup.el-for-current-line)
                     ))

  ;; Minibuf に出力
  (defun my-flymake-display-err-minibuf-for-current-line ()
    "Displays the error/warning for the current line in the minibuffer"
    (interactive)
    (let* ((line-no             (flymake-current-line-no))
           (line-err-info-list  (nth 0 (flymake-find-err-info flymake-err-info line-no)))
           (count               (length line-err-info-list)))
      (while (> count 0)
        (when line-err-info-list
          (let* ((text       (flymake-ler-text (nth (1- count) line-err-info-list)))
                 (line       (flymake-ler-line (nth (1- count) line-err-info-list))))
            (message "[%s] %s" line text)))
        (setq count (1- count)))))

  ;; popup.el を使って tip として表示
  (defun my-flymake-display-err-popup.el-for-current-line ()
    "Display a menu with errors/warnings for current line if it has errors and/or warnings."
    (interactive)
    (let* ((line-no             (flymake-current-line-no))
           (line-err-info-list  (nth 0 (flymake-find-err-info flymake-err-info line-no)))
           (menu-data           (flymake-make-err-menu-data line-no line-err-info-list)))
      (if menu-data
          (popup-tip (mapconcat '(lambda (e) (nth 0 e))
                                (nth 1 menu-data)
                                "\n")))
      ))

  (defun flymake-simple-generic-init (cmd &optional opts)
    (let* ((temp-file   (flymake-init-create-temp-buffer-copy
                         'flymake-create-temp-inplace))
           (local-file  (file-relative-name
                         temp-file
                         (file-name-directory buffer-file-name))))
      (list cmd (append opts (list local-file)))))

  ;; Makefile が無くてもC/C++のチェック
  (defun flymake-simple-make-or-generic-init (cmd &optional opts)
    (if (file-exists-p "Makefile")
        (flymake-simple-make-init)
      (flymake-simple-generic-init cmd opts)))

  (defun flymake-c-init ()
    (flymake-simple-make-or-generic-init
     "gcc" '("-Wall" "-Wextra" "-pedantic" "-fsyntax-only")))

  (defun flymake-cc-init ()
    (flymake-simple-make-or-generic-init
     "g++" '("-Wall" "-Wextra" "-pedantic" "-fsyntax-only")))

  (push '("\\.[cCmM]\\'" flymake-c-init) flymake-allowed-file-name-masks)
  (push '("\\.\\(?:cc\|cpp\|CC\|CPP\\)\\'" flymake-cc-init) flymake-allowed-file-name-masks)

  ;; FlymakeTailorRuby
  (when (executable-find "tailor")
    (defun flymake-tailor-ruby-init ()
      (let* ((temp-file   (flymake-init-create-temp-buffer-copy
                           'flymake-create-temp-inplace))
             (local-file  (file-relative-name
                           temp-file
                           (file-name-directory buffer-file-name))))
        (list "tailor" (list "-d" local-file))))

    (push '(".+\\.rb$" flymake-tailor-ruby-init) flymake-allowed-file-name-masks)
    (push '("^.*?<Problem> ?+\\([0-9]+\\)\\[\\([0-9]+\\)\\] ?+: ?+ERROR\\[.*?\\] ?+\\(.*?\\)$" nil 1 2 3) flymake-err-line-patterns)
    )

  ;; Invoke ruby with '-c' to get syntax checking
  (when (executable-find "ruby")
    (defun flymake-ruby-init ()
      (flymake-simple-generic-init
       "ruby" '("-c")))

    (push '(".+\\.rb\\'" flymake-ruby-init) flymake-allowed-file-name-masks)
    (push '(".+\\.rake\\'" flymake-ruby-init) flymake-allowed-file-name-masks)
    (push '("Rakefile\\'" flymake-ruby-init) flymake-allowed-file-name-masks)

    (push '("^\\(.*\\):\\([0-9]+\\): \\(.*\\)$" 1 2 nil 3) flymake-err-line-patterns)
    )

  ;; bash チェック
  (defvar flymake-shell-of-choice
    "bash"
    "Path of shell.")

  (defvar flymake-shell-arguments
    (list "-n")
    "Shell arguments to invoke syntax checking.")

  (defun flymake-shell-init ()
    (flymake-simple-generic-init
     flymake-shell-of-choice flymake-shell-arguments))

  (push '(".+\\.sh\\'" flymake-shell-init) flymake-allowed-file-name-masks)
  (push '("^\\(.+\\): line \\([0-9]+\\): \\(.+\\)$" 1 2 nil 3) flymake-err-line-patterns)

  )

;; flymake-ruby
(require 'flymake-ruby)

;; motion-mode
(require 'motion-mode)

(add-hook 'ruby-mode-hook 'motion-recognize-project)
(add-to-list 'ac-modes 'motion-mode)
(add-to-list 'ac-sources 'ac-source-dictionary)

(define-key motion-mode-map (kbd "C-c C-c") 'motion-execute-rake)
(define-key motion-mode-map (kbd "C-c C-d") 'motion-dash-at-point)
(define-key motion-mode-map (kbd "C-c C-p") 'motion-convert-code-region)

;; dash-at-point
(require 'dash-at-point)
(define-key global-map (kbd "C-c C-d") 'dash-at-point)
(add-hook 'rinari-minor-mode-hook
          (lambda () (setq dash-at-point-docset "rails")))

;; js2-mode
(require 'js2-mode)

(add-to-list 'auto-mode-alist '("\\.js$" . js2-mode))
(add-hook 'js2-mode-hook
          '(lambda ()
             (setq js2-basic-offset 4)))

;; ag
(require 'ag)
(setq ag-highlight-search t)
(defun set-exec-path-from-shell-PATH ()
  "Set up Emacs' `exec-path' and PATH environment variable to match that used by the user's shell.

This is particularly useful under Mac OSX, where GUI apps are not started from a shell."
  (interactive)
  (let ((path-from-shell (replace-regexp-in-string "[ \t\n]*$" "" (shell-command-to-string "$SHELL --login -i -c 'echo $PATH'"))))
    (setenv "PATH" path-from-shell)
    (setq exec-path (split-string path-from-shell path-separator))))

(set-exec-path-from-shell-PATH)

;; region-bindings-mode
(require 'region-bindings-mode)
(region-bindings-mode-enable)

;; mutiple-cursor
(require 'multiple-cursors)
(define-key region-bindings-mode-map "a" 'mc/mark-all-like-this)
(define-key region-bindings-mode-map "p" 'mc/mark-previous-like-this)
(define-key region-bindings-mode-map "n" 'mc/mark-next-like-this)
(define-key region-bindings-mode-map "m" 'mc/mark-more-like-this-extended)

(global-unset-key (kbd "M-<down-mouse-1>"))
(global-set-key (kbd "M-<mouse-1>") 'mc/add-cursor-on-click)

;; rhtml-mode
(require 'rhtml-mode)

;; exec-path-from-shell
(require 'exec-path-from-shell)
(exec-path-from-shell-initialize)


;; elisp の設定
(define-key global-map (kbd "C-c C-g") 'ginger-region)

;; 自分の elisp の設定
(load-file "~/.emacs.d/site-lisp/dash.el")
(load-file "~/.emacs.d/site-lisp/move-line.el")
(load-file "~/.emacs.d/site-lisp/ginger-api.el")
(load-file "~/.emacs.d/site-lisp/copy-and-paste-for-osx.el")
(load-file "~/.emacs.d/site-lisp/reopen-file.el")
(load-file "~/.emacs.d/site-lisp/hlline-face.el")
(load-file "~/.emacs.d/site-lisp/helm-git-project.el")
