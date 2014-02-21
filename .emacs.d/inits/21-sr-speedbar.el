(require 'sr-speedbar)

;; sr-speedbar config
(setq sr-speedbar-right-side nil)
(setq sr-speedbar-skip-other-window-p t)
(setq sr-speedbar-auto-refresh t)
(setq sr-speedbar-max-width 20)

;; regular speedbar config
(setq speedbar-show-unknown-files t)
(setq speedbar-verbosity-level 0)
;;(setq speedbar-use-images nil)

;; open it on startup
;; needs to be called in this hook for proper sizing
;;(add-hook 'window-setup-hook (lambda () (sr-speedbar-open) (other-window 1)))



(defadvice delete-other-windows (after my-sr-speedbar-delete-other-window-advice activate)
  "Check whether we are in speedbar, if it is, jump to next window."
  (let ()
    (when (and (sr-speedbar-window-exist-p sr-speedbar-window)
               (eq sr-speedbar-window (selected-window)))
      (other-window 1)
    )))
(ad-enable-advice 'delete-other-windows 'after 'my-sr-speedbar-delete-other-window-advice)
(ad-activate 'delete-other-windows)

;;(defadvice around-delete-other-windows (around around-delete-other-windows)
;;  "Closes and reopens speedbar to avoid channging speedbars width."
;;  (sr-speedbar-close)
;;  ad-do-it
;;  (sr-speedbar-open))
;;(ad-enable-advice 'delete-other-windows 'around 'around-delete-other-windows)
;;(ad-activate 'around-delete-other-windows)

(defun speedbar-edit-line-and-switch-to-window ()
  (interactive)
  (speedbar-edit-line)
  (other-window 1))

(defun speedbar-open-mac ()
  (interactive)
  (let ((file-name (speedbar-line-file)))
    (if (file-exists-p file-name)
        (call-process "/usr/bin/open" nil 0 nil file-name))))

(define-key speedbar-file-key-map "+" 'speedbar-create-directory)
(define-key speedbar-file-key-map (kbd "O") (lambda () (interactive) (speedbar-open-mac)))
(define-key speedbar-file-key-map (kbd "o") (lambda () (interactive) (speedbar-edit-line-and-switch-to-window)))
(define-key speedbar-file-key-map (kbd "f") (lambda () (interactive) (speedbar-edit-line)))
(define-key speedbar-file-key-map (kbd "RET") (lambda () (interactive) (speedbar-edit-line)))
