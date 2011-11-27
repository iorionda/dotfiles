; -*- Mode: Emacs-Lisp ; Coding: utf-8-emacs-unix ; indent-tabs-mode: nil -*-
;; init.el -- Emacs Setting elisp file
;; Copyright (C) 2010  Iori ONDA

;; Author: Iori ONDA <iorionda@gmail.com>
;; Keywords: tools

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, 
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;;; Code:

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Profile
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq user-full-name "Iori ONDA"
      user-mail-address "iori.onda@me.com")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; load-path 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun add-to-load-path(&rest paths)
  (mapc '(lambda (path)
	   (add-to-list 'load-path path))
	(mapcar 'expand-file-name paths)))

(add-to-load-path
 "~/.emacs.d"
 "~/.emacs.d/config/packages")

(setq default-directory "~/")
(setq inhibit-startup-message t)
(setq inhibit-scratch-message nil)
(fset 'yes-or-no-p 'y-or-n-p)
(setq transient-mark-mode t)
(setq search-hilight t)
(setq-default tab-width 4 indent-tabs-mode nil)
;;(set-message-beep 'silent)
(setq visble-bell t)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Japanesse Language
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(set-locale-environment nil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; key-bind
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Command-keyとOption-keyの入れ替え
(setq ns-command-modifier (quote meta))
(setq ns-alternate-modifier (quote super))

(setq mac-pass-control-to-system nil)
(setq mac-pass-command-to-system nil)
(setq mac-pass-option-to-system nil)

(define-key global-map(kbd "C-h") 'delete-backward-char)
(define-key global-map(kbd "M-?") 'help-for-help)
(define-key global-map(kbd "C-z") 'undo)
(define-key global-map(kbd "C-c i") 'indent-region)
(define-key global-map(kbd "C-c C-i") 'hippie-expand)
(define-key global-map(kbd "C-c ;") 'comment-dwim)
(define-key global-map(kbd "C-o") 'toggle-input-method)
(define-key global-map(kbd "M-C-g") 'grep)
(define-key global-map(kbd "C-[ M-C-g") 'goto-line)
(define-key global-map(kbd "C-x C-j") 'skk-mode)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; grep
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'grep)
(setq grep-command-before-query "grep -nH -e -r")
(defun grep-default-command ()
  (if current-prefix-arg
      (let ((grep-command-before-target
	     (concat grep-command-before-query
		     (shell-quote-argument (grep-tag-default)))))
	(cons (if buffer-file-name
		  (concat grep-command-before-target
			  " *."
			  (file-name-extention buffer-file-name))
		(concat grep-command-before-target " ."))
	      (+ (length grep-command-before-target) 1 )))
    (car grep-command)))
(setq grep-command (cons (concat grep-command-before-query " .")
			 (+ (length grep-command-before-query) 1)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; display image
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(auto-image-file-mode t)

;; eval
(setq eval-expression-print-length nil)

;; paren
(show-paren-mode 1)
(setq show-paren-style 'mixed)

;; blank
(when 
    (boundp 'show-trailling-whitespace)
  (setq-default show-trailling-whitespace t))

;; cursor
(blink-cursor-mode 0)
(set-cursor-color "orange")
(setq blink-cursor-interval 0.5)
(setq blink-cursor-delay 1.0)
(blink-cursor-mode 1)

(defface hlline-face
  '((((class color)
      (background dark))
     (:background "dark slate gray"))
    (((class color)
      (background light))
     (:background "ForestGreen"))
    (t
     ()))
  "*Face used by hl-line.")

(setq hl-line-face 'hlline-face)
(setq hl-line-face 'underline) ; 下線
(global-hl-line-mode)

(column-number-mode t)
(line-number-mode t)
(require 'saveplace)
(setq-default save-place t)

;; line
(setq kill-whole-line t)
(setq require-final-newline t)
(setq next-line-add-newlines nil)

;; backup
(setq backup-inhibited t)
(setq delete-auto-save-file t)

;; save
(setq auto-save-default nil)
(setq auto-save-list-file-prefix nil)

;; complication
(setq complication-ignore-case t)
(setq read-file-name-complication-ignore-case t)

(partial-completion-mode t)
(icomplete-mode 1)

;; history
(setq history-length 10000)
(savehist-mode 1)
(setq recentf-max-saved-items 10000)

;; compression
(auto-compression-mode t)

;; diff
(setq ediff-window-setup-function 'ediff-setup-window-plain)
(setq diff-switches '("-u" "-p" "-N"))

;; directory
(require 'dired-x)
(require 'wdired)
(define-key dired-mode-map "r" 'wdired-change-to-wdired-mode)

;; buffer
(require 'uniquify)
(setq uniquify-buffer-name-style 'post-forward-angle-brackets)
(setq uniquify-ignore-buffers-re "*[^*]+*")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; face
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(if window-system
    (progn
      (setq initial-frame-alist '(
                                  (width . 290)
                                  (height . 100)
                                  (top . 0)
                                  (left . 0)
                                  (alpha . 80)))
      ;; face
      (set-background-color "black")
      (set-foreground-color "alice blue")
      (menu-bar-mode nil)
      (tool-bar-mode -1)
      (scroll-bar-mode -1)
      (setq frame-title-format (format "emacs@%s : %%f" (system-name)))
      ;; font
      ;;システム依存を排除するために一旦デフォルトフォントセットを上書き 
      (set-face-attribute  'default nil :family "Ricty"  :height 160)
      (set-frame-font "ricty-13.5")
      (add-to-list 'default-frame-alist '(font . "ricty-13.5"))
      ;; CJK Symbols and Punctuation  3000-303F http://www.triggertek.com/r/unicode/3000-303F
      ;; Hiragana                     3040-309f http://www.triggertek.com/r/unicode/3040-309F
      ;; Katakana                     30a0-30ff http://www.triggertek.com/r/unicode/30A0-30F
      (set-fontset-font nil
                        'unicode
                        (font-spec :family "M+ 1mn")
                        nil
                        'append)
      (set-fontset-font nil
                        '( #x3000 .  #x30ff)
                        (font-spec :family "M+ 1mn" :style "regular")
                        nil
                        'prepend)
      (set-fontset-font nil
                        '( #xff00 .  #xffef)
                        (font-spec :family "M+ 1mn" :style "regular")
                        nil
                        'prepend)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; info-path
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'info)
(add-to-list 'Info-additional-directory-list "~/.emacs.d/info")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; recentf
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'recentf)
;; 最近利用されたファイル500個を対象とする.
(setq recentf-max-saved-items 500)
;; 最近使ったファイルに加えないファイルを正規表現で指定する.
(setq recent-exclude '("/TAGS$/" "/var/tmp"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; *scratch* buffer の自動復旧 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun my-make-scratch (&optional arg)
  (interactive)
  (progn
    ;; "*scratch*" を作成して buffer-list に放り込む
    (set-buffer (get-buffer-create "*scratch*"))
    (funcall initial-major-mode)
    (erase-buffer)
    (when (and initial-scratch-message (not inhibit-startup-message))
      (insert initial-scratch-message))
    (or arg (progn (setq arg 0)
                   (switch-to-buffer "*scratch*")))
    (cond ((= arg 0) (message "*scratch* is cleared up."))
          ((= arg 1) (message "another *scratch* is created")))))

(add-hook 'kill-buffer-query-functions
	  ;; *scratch* バッファで kill-buffer したら内容を消去するだけにする
          (lambda ()
            (if (string= "*scratch*" (buffer-name))
                (progn (my-make-scratch 0) nil)
              t)))

(add-hook 'after-save-hook
	  ;; *scratch* バッファの内容を保存したら *scratch* バッファを新しく作る
         (lambda ()
            (unless (member (get-buffer "*scratch*") (buffer-list))
              (my-make-scratch 1))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; linum-mode
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(if (window-system)
    (require 'linum)
  (global-linum-mode t)
  (global-set-key [f5] 'linum-mode)
  (defvar my-linum-min-width 4)
  (setq linum-format
        (lambda (line)
          (let ((fmt (format
                      "%%%dd"
                      (max
                       my-linum-min-width
                       (length (number-to-string
                                (count-lines (point-min) (point-max))))))))
            (propertize (format fmt line) 'face 'linum)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; auto-fill-mode
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(auto-fill-mode nil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; abbrev
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 保存先を指定する
(setq abbrev-file-name "~/.abbrev_defs")
;; 略称展開のキーバインドを指定する
(define-key esc-map " " 'expand-abbrev)
;; 起動時に保存した略称を読み込む
(quietly-read-abbrev-file)
;; 略称を保存する
(setq save-abbrevs t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ddskk
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(let ((default-directory (expand-file-name "~/.emacs.d/lisp")))
 (add-to-list 'load-path default-directory)
 (if (fboundp 'normal-top-level-add-subdirs-to-load-path)
     (normal-top-level-add-subdirs-to-load-path)))

;; (add-to-load-path "~/.emacs.d/lisp/skk")
(require 'skk-autoloads)
(require 'skk-setup)

;; 辞書サーバの設定
(setq skk-server-host "localhost")
(setq skk-server-portnum 1178)

;; Emacs の起動時に必要な物をロードすることで、SKK の初回起動を速くする
(setq skk-preload t)

;; 表示の設定
;; メッセージを日本語で表示する
(setq skk-japanese-message-and-error t)

;; 変換時に注釈(annotation)を付ける
(setq skk-show-attotation t)

;; jpを設定すると日本語の句点読点を利用する
;;(setq-default skk-kutouten-type 'jp)
(setq-default skk-kutouten-type 'en)

;; 送り仮名が厳密に正しい方を優先して設定する.
(setq skk-henkan-strict-okuri-precedence t)

;; 辞書登録の時に余計な送り仮名を登録しないようにする
(setq skk-check-okurigana-on-touroku 'auto)

;; C-\ でも SKK に切り替えられるように設定
(setq default-input-method "japanese-skk")

;;  C-j の機能を別のキーに割り当て
(global-set-key (kbd "C-m") 'newline-and-indent)

;; ローカル辞書を使う時の設定
;; (setq skk-large-jisyo "~/.emacs.d/share/skk/SKK-JISYO.L")
(global-set-key "\C-x\C-j" 'skk-mode)
(global-set-key "\C-xj" 'skk-auto-fill-mode)
(global-set-key "\C-xt" 'skk-tutorial)

;; toggleのON/OFFの切り替え
(global-set-key "\C-c\C-l" 'toggle-truncate-lines)

;;; 追加の設定
;; 標準Elispの設定
(load "config/builtins")
;; 非標準Elispの設定
(load "config/packages")
