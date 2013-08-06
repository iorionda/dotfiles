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
;; │             罫線                                         |
;; └─────────────────────────────┘
;; font
(if window-system
    (progn
      (set-fontset-font
       nil 'japanese-jisx0208
       (font-spec :family "Ricty"))
      (create-fontset-from-ascii-font "Ricty-14:weight=normal:slant=normal" nil "ricty")
      (set-fontset-font "fontset-ricty"
                        'japanese-jisx0208
                        (font-spec :family "Ricty" :size 14)
                        nil
                        'append)
      (add-to-list 'default-frame-alist '(font . "fontset-ricty"))))
