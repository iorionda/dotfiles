;; M-x package-install RET request RET

(require 'request)
(require 'json)

(defvar ginger-end-point
  "http://services.gingersoftware.com/Ginger/correct/json/GingerTheText"  )

;;;###autoload
(defun ginger-region (beg end)
  (interactive "r")
  (lexical-let* ((text (buffer-substring-no-properties beg end))
                 (results nil))
    (request
     ginger-end-point
     :params `((lang . "US")
               (clientVersion . "2.0")
               (apiKey . "6ae0c3a0-afdc-4532-a810-82ded0054236")
               (text . ,text))
     :parser 'json-read
     :success (function*
               (lambda (&key data &allow-other-keys)
                 (loop with elems = (assoc-default 'LightGingerTheTextResult data)
                       with i = 0
                       for elem across elems
                       for from = (assoc-default 'From elem)
                       for to   = (assoc-default 'To elem)
                       for suggest = (assoc-default
                                      'Text (aref (assoc-default 'Suggestions elem) 0))
                       do
                       (progn
                         (when (< i from)
                           (push (substring text i from) results))
                         (push (propertize suggest
                                           'face 'error) results)
                         (setq i (1+ to)))
                       finally
                       (when (< i (length text))
                         (push (substring text i) results)))
                 (pop-to-buffer (get-buffer-create "*ginger*"))
                 (insert (concat "[Original]\n" text "\n"))
                 (insert (concat "[Fixed]\n"
                                 (mapconcat 'identity (reverse results) "")
                                 "\n")))))))
