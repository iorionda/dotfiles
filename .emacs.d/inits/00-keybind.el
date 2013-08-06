;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;キーバインド
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(keyboard-translate ?\C-h ?\C-?)

;; 基本
(define-key global-map (kbd "M-?") 'help-for-help)        ; ヘルプ
(define-key global-map (kbd "C-c i") 'indent-region)      ; インデント
(define-key global-map (kbd "C-c C-i") 'hippie-expand)    ; 補完
(define-key global-map (kbd "C-c ;") 'comment-dwim)       ; コメントアウト
(define-key global-map (kbd "M-C-g") 'grep)               ; grep
(define-key global-map (kbd "C-h") 'delete-backward-char)
(define-key global-map (kbd "M-?")  'help-for-help)
(define-key global-map (kbd "C-\\") 'undo)
(define-key global-map (kbd "C-c C-i") 'dabbrev-expand)
(define-key global-map (kbd "C-c g") 'goto-line)

;; 改行したらindentする
(define-key global-map (kbd "RET") 'newline-and-indent)

;;;ウィンドウ移動
;;次のウィンドウへ移動
(define-key global-map (kbd "C-M-n") 'next-multiframe-window)
;;前のウィンドウへ移動
(define-key global-map (kbd "C-M-p") 'previous-multiframe-window)
