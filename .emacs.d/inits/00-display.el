;; カラーテーマのロードパスを設定
(add-to-list 'custom-theme-load-path "~/.emacs.d/color-themes")
(load-theme 'molokai t nil)

;; フレーム透過設定
(add-to-list 'default-frame-alist '(alpha . (0.95 0.95)))

;スクロールバーを消す
(set-scroll-bar-mode 'nil)
;ツールバーを消す
(tool-bar-mode -1)

;; 起動時にフルスクリーンにする
;; (add-hook 'window-setup-hook 'ns-toggle-fullscreen)

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
