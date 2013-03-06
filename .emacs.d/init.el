;; -*- Mode: Emacs-Lisp ; Coding: utf-8 -*-
(require 'cl)

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;キーバインド
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(keyboard-translate ?\C-h ?\C-?)

;; 基本
(define-key global-map (kbd "M-?") 'help-for-help)        ; ヘルプ
(define-key global-map (kbd "C-z") 'undo)                 ; undo
(define-key global-map (kbd "C-c i") 'indent-region)      ; インデント
(define-key global-map (kbd "C-c C-i") 'hippie-expand)    ; 補完
(define-key global-map (kbd "C-c ;") 'comment-dwim)       ; コメントアウト
(define-key global-map (kbd "M-C-g") 'grep)               ; grep

;;;ウィンドウ移動
;;次のウィンドウへ移動
(define-key global-map (kbd "C-M-n") 'next-multiframe-window)
;;前のウィンドウへ移動
(define-key global-map (kbd "C-M-p") 'previous-multiframe-window)

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

;; フォント
;; abcdefghijklmnopqrstuvwxyz
;; ABCDEFGHIJKLMNOPQRSTUVWXYZ
;; `1234567890-=\[];',./
;; ~!@#$%^&*()_+|{}:"<>?
;;
;; 壱弐参四五壱弐参四五壱弐参四五壱弐参四五壱弐参四五壱弐参四五
;; 123456789012345678901234567890123456789012345678901234567890
;; ABCdeABCde
;;
;; ┌─────────────────────────────┐
;; │             罫線                            │
;; └─────────────────────────────┘
;;

(set-face-attribute 'default nil :family "Ricty" :height 130)
(set-fontset-font nil 'japanese-jisx0208 (font-spec :family "Ricty"))
(setq face-font-rescale-alist '(("MeiryoKe_Console" . 1.2)))

;;;外観
(load-theme 'railscasts t nil)

;; フレーム透過設定
(add-to-list 'default-frame-alist '(alpha . (0.95 0.95)))

;スクロールバーを消す
(set-scroll-bar-mode 'nil)
;ツールバーを消す
(tool-bar-mode 'nil)

;;;対応する括弧を光らせる
(setq show-paren-delay 0)
(setq show-paren-style 'single)
(show-paren-mode t)
;;; ウィンドウ内に収まらないときだけ括弧内も光らせる。
(setq show-paren-style 'mixed)

;; 空白や長すぎる行を視覚化する。
(require 'whitespace)
;; 1行が80桁を超えたら長すぎると判断する。
(setq whitespace-line-column 80)
(setq whitespace-style '(face              ; faceを使って視覚化する。
                         trailing          ; 行末の空白を対象とする。
                         lines-tail        ; 長すぎる行のうち
                                           ; whitespace-line-column以降のみを
                                           ; 対象とする。
                         space-before-tab  ; タブの前にあるスペースを対象とする。
                         space-after-tab)) ; タブの後にあるスペースを対象とする。
;; デフォルトで視覚化を有効にする。
(global-whitespace-mode 1)

;;;行番号の表示
(global-linum-mode t)
(setq linum-format "%4d:")
;;; カーソルの位置が何文字目かを表示する
(column-number-mode t)
;;; カーソルの位置が何行目かを表示する
(line-number-mode t)
;;; カーソルの場所を保存する
(require 'saveplace)
(setq-default save-place t)

;;;バックアップ・オートセーブファイルの作成をやめる
(setq make-backup-files nil)
(setq auto-save-default nil)

;; 基本

;; 行
;;; 行の先頭でC-kを一回押すだけで行全体を消去する
(setq kill-whole-line t)
;;; 最終行に必ず一行挿入する
(setq require-final-newline t)
;;; バッファの最後でnewlineで新規行を追加するのを禁止する
(setq next-line-add-newlines nil)

;;; バックアップファイルを作らない
(setq backup-inhibited t)
;;; 終了時にオートセーブファイルを消す
(setq delete-auto-save-files t)

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

;;;----------------------------------------------
;;;パッケージ管理
;;;package.el
;;;please execute M-x list-package at first
(require 'package)
(add-to-list 'package-archives
	     '("melpa" . "http://melpa.milkbox.net/packages/") t)
(add-to-list 'package-archives
	     '("marmalade" . "http://marmalade-repo.org/packages/"))
(package-initialize)

;;;popwin.el :pop up window for emacs baffer
;;M-x package-install RET popwin
(require 'popwin)
(setq display-buffer-function 'popwin:display-buffer)
(setq popwin:popup-window-height 0.5)
(defvar popwin:special-display-config-backup popwin:special-display-config)
(setq popwin:special-display-config
      (append '(("*Remember*" :stick t)("*Org Agenda*")("*Backtrace*")
                ("*sdic*" :noselect))
              popwin:special-display-config))
(define-key global-map (kbd "C-x p") 'popwin:display-last-buffer)

;;;helm.el
;;M-x package-install RET helm RET
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

(setq helm-samewindow nil)
(push '("*helm-M-x*") popwin:special-display-config)

;; emacsの終了時に、履歴を保存する
(remove-hook 'kill-emacs-hook 'helm-c-adaptive-save-history)
;; ディレイは0.2秒
(setq helm-input-idle-delay 0.02)
;; 候補のディレクトリが一つしかない場合に、自動的に展開しない
(setq helm-ff-auto-update-initial-value nil)

;;;powerline
;M-x package-install RET powerline
(require 'powerline)
(defun arrow-right-xpm (color1 color2)
  "Return an XPM right arrow string representing."
  (format "/* XPM */
static char * arrow_right[] = {
\"12 18 2 1\",
\". c %s\",
\"  c %s\",
\".           \",
\"..          \",
\"...         \",
\"....        \",
\".....       \",
\"......      \",
\".......     \",
\"........    \",
\".........   \",
\"..........  \",
\"........... \",
\"............\",
\"........... \",
\"..........  \",
\".........   \",
\"........    \",
\".......     \",
\"......      \",
\".....       \",
\"....        \",
\"...         \",
\"..          \",
\".           \",
\"            \"};"  color1 color2))

(defun arrow-left-xpm (color1 color2)
  "Return an XPM right arrow string representing."
  (format "/* XPM */
static char * arrow_right[] = {
\"12 18 2 1\",
\". c %s\",
\"  c %s\",
\"           .\",
\"          ..\",
\"         ...\",
\"        ....\",
\"       .....\",
\"      ......\",
\"     .......\",
\"    ........\",
\"   .........\",
\"  ..........\",
\" ...........\",
\"............\",
\" ...........\",
\"  ..........\",
\"   .........\",
\"    ........\",
\"     .......\",
\"      ......\",
\"       .....\",
\"        ....\",
\"         ...\",
\"          ..\",
\"           .\",
\"            \"};"  color2 color1))

(defconst color1 "#0044cc")
(defconst color2 "#0088cc")
(defconst color3 "#696969")
(defconst color4 "#FF0066")
(defconst color5 "#CDC0B0")

(defvar arrow-right-1 (create-image (arrow-right-xpm color1 color2)
				    'xpm t :ascent 'center))
(defvar arrow-right-2 (create-image (arrow-right-xpm color2 color3)
				    'xpm t :ascent 'center))
(defvar arrow-right-3 (create-image (arrow-right-xpm color3 "None")
				    'xpm t :ascent 'center))
(defvar arrow-left-1  (create-image (arrow-left-xpm color2 color1)
				    'xpm t :ascent 'center))
(defvar arrow-left-2  (create-image (arrow-left-xpm "None" color2)
				    'xpm t :ascent 'center))

(setq-default mode-line-format
 (list  '(:eval (concat (propertize " %* %b " 'face 'mode-line-color-1)
                        (propertize " " 'display arrow-right-1)))
        '(:eval (concat (propertize " %Z " 'face 'mode-line-color-2)
                        (propertize " " 'display arrow-right-2)))
        '(:eval (concat (propertize " %m " 'face 'mode-line-color-3)
                        (propertize " " 'display arrow-right-3)))

        ;; Justify right by filling with spaces to right fringe - 16
        ;; (16 should be computed rahter than hardcoded)
        '(:eval (propertize " " 'display
			    '((space :align-to (- right-fringe 16)))))

        '(:eval (concat (propertize " " 'display arrow-left-2)
                        (propertize " %p " 'face 'mode-line-color-2)))
        '(:eval (concat (propertize " " 'display arrow-left-1)
                        (propertize "%4l:%2c  " 'face 'mode-line-color-1)))
))

(make-face 'mode-line-color-1)
(set-face-attribute 'mode-line-color-1 nil
                    :foreground "#fffacd"
                    :background color1)

(make-face 'mode-line-color-2)
(set-face-attribute 'mode-line-color-2 nil
                    :foreground "#fffacd"
                    :background color2)

(make-face 'mode-line-color-3)
(set-face-attribute 'mode-line-color-3 nil
                    :foreground "#fffacd"
                    :background color3)

(set-face-attribute 'mode-line nil
                    :foreground "#fffacd"
                    :background color4
		    :bold t
                    :box nil)
(set-face-attribute 'mode-line-inactive nil
                    :foreground "#fffacd"
                    :background color5)

;;;magit
;M-x package-install RET magit
(require 'magit)
;; 色変更
(set-face-foreground 'magit-diff-add "#b9ca4a") ; 追加した部分を緑に
(set-face-foreground 'magit-diff-del "#d54e53")  ; 削除した 部分を赤に
(set-face-background 'magit-item-highlight "#000000") ; 選択項目ハイライトがうっとうしいので背景色と同化

;;;ruby-mode
;M-x package-install RET ruby-mode
(require 'ruby-mode)

;;;ruby-end
;M-x package-install RET ruby-end
(require 'ruby-end)
(add-hook 'ruby-mode-hook
  '(lambda ()
    (abbrev-mode 1)
    (electric-pair-mode t)
    (electric-indent-mode t)
    (electric-layout-mode t)))

