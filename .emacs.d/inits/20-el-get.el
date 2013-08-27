(setq el-get-generate-autoloads nil)

(setq el-get-dir "~/.emacs.d/elisp/el-get")

(unless (require 'el-get nil 'noerror)
  (with-current-buffer
      (url-retrieve-synchronously
       "https://raw.github.com/dimitri/el-get/master/el-get-install.el")
    (goto-char (point-max))
    (eval-print-last-sexp)))

(el-get 'sync)

;; package to install from el-get
(defvar el-get-packages
  '(
    howm
    )
  "A list of packages to install from el-get at launch.")

(el-get 'sync el-get-packages)
