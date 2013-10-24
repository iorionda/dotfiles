;;;magit
(require 'magit)
;; 色変更

; 追加した部分を緑に
(set-face-foreground 'magit-diff-add "#b9ca4a")
; 削除した 部分を赤に
(set-face-foreground 'magit-diff-del "#d54e53")
; 選択項目ハイライトがうっとうしいので背景色と同化
(set-face-background 'magit-item-highlight "#000000")
