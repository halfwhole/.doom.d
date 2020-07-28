(eval-when-compile (require 'cl))

(defconst +xxx-directory+ (file-name-directory (or load-file-name buffer-file-name)))

(defconst +xxx-rainbow-image+ (concat +xxx-directory+ "img/rainbow.xpm"))
(defconst +xxx-outerspace-image+ (concat +xxx-directory+ "img/outerspace.xpm"))

(defconst +xxx-modeline-help-string+ "Help string!\nmouse-1: Scroll buffer position")

(defvar xxx-old-car-mode-line-position nil)

(defgroup xxx nil
  "TEMP"
  :group 'frames)

(defun xxx-refresh ()
  "TEMP"
  (when (featurep 'xxx-mode)
    (when (and (boundp 'xxx-mode)
               xxx-mode)
      (xxx-mode -1)
      (xxx-mode 1))))

(defcustom xxx-minimum-window-width 0
  "TEMP"
  :type 'integer
  :set (lambda (sym val)
         (set-default sym val)
         (xxx-refresh))
  :group 'xxx)

(defcustom xxx-bar-length 0
  "TEMP"
  :type 'integer
  :set (lambda (sym val)
         (set-default sym val)
         (xxx-refresh))
  :group 'xxx)

(defvar xxx-minimum-window-width 64)
(defvar xxx-bar-length 64)

(defun xxx-number-of-rainbows ()
  (round (/ (* (round (* 100
                         (/ (- (float (point))
                               (float (point-min)))
                            (float (point-max)))))
               xxx-bar-length)
            100)))

(defun xxx-scroll-buffer (percentage buffer)
  (with-current-buffer buffer
    (goto-char (floor (* percentage (point-max))))))

(defun xxx-add-scroll-handler (string percentage buffer)
  (lexical-let ((percentage percentage)
                (buffer buffer))
    (propertize string 'keymap `(keymap (mode-line keymap (down-mouse-1 . ,(lambda () (interactive) (xxx-scroll-buffer percentage buffer))))))))

(defun xxx-create ()
  (if (< (window-width) xxx-minimum-window-width)
      ""                                ; disabled for too small windows
    (let* ((rainbows (xxx-number-of-rainbows))
           (outerspaces (- xxx-bar-length rainbows))
           (rainbow-string "")
           (xpm-support (image-type-available-p 'xpm))
           (outerspace-string "")
           (buffer (current-buffer)))
      (dotimes (number rainbows)
        (setq rainbow-string (concat rainbow-string
                                     (xxx-add-scroll-handler
                                      (if xpm-support
                                          (propertize "|"
                                                      'display (create-image +xxx-rainbow-image+ 'xpm nil :ascent 'center))
                                        "|")
                                      (/ (float number) xxx-bar-length) buffer))))
      (dotimes (number outerspaces)
        (setq outerspace-string (concat outerspace-string
                                        (xxx-add-scroll-handler
                                         (if xpm-support
                                             (propertize "-"
                                                         'display (create-image +xxx-outerspace-image+ 'xpm nil :ascent 'center))
                                           "-")
                                         (/ (float (+ rainbows number)) xxx-bar-length) buffer))))
      (propertize (concat rainbow-string outerspace-string) 'help-echo +xxx-modeline-help-string+))))

;;;###autoload
(define-minor-mode xxx-mode
  "TEMP"
  :global t
  :group 'xxx
  (if xxx-mode
      (progn
        (unless xxx-old-car-mode-line-position
          (setq xxx-old-car-mode-line-position (car mode-line-position)))
        (setcar mode-line-position '(:eval (list (xxx-create)))))
    (setcar mode-line-position xxx-old-car-mode-line-position)))

;;;###autoload
(define-minor-mode xxx-mode
  "TEMP"
  :global t
  :group 'xxx
  (if xxx-mode
      (progn
        (unless xxx-old-car-mode-line-position
          (setq xxx-old-car-mode-line-position (car mode-line-position)))
        (setcar mode-line-position '(:eval (list (xxx-create)))))
    (setcar mode-line-position xxx-old-car-mode-line-position)))

(provide 'xxx-mode)
