(defun dash ()
  (interactive)
  (shell-command
   (format "open dash://%s"
           (or (thing-at-point 'symbol) ""))))
