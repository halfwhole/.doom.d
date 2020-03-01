;; Loads a random quote from `quotes-text-file` to the dashboard every startup

(setq quotes-text-file "~/.doom.d/personal/quotes.txt")

(setq +doom-dashboard-functions
      (cons 'my-dashboard-widget-banner
            (delete 'doom-dashboard-widget-banner +doom-dashboard-functions)))

(defun read-file (f)
  (with-temp-buffer
    (insert-file-contents f)
    (buffer-substring-no-properties
       (point-min)
       (point-max))))

(setq selected-quote
     (let* ((raw-text (read-file quotes-text-file))
            (all-quotes-text (split-string raw-text "\n\n+" t))
            (all-quotes-lines (mapcar (lambda (quote) (split-string quote "\n" t)) all-quotes-text))
            (selected-quote (elt all-quotes-lines (random (length all-quotes-lines)))))
       selected-quote))

(defun my-dashboard-widget-banner ()
  (mapc (lambda (line)
          (insert (propertize (+doom-dashboard--center +doom-dashboard--width line)
                              'face 'doom-dashboard-menu-desc) " ")
          (insert "\n"))
        selected-quote)
  (insert "\n"))
