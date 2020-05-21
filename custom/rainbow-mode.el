;;; nyan-mode.el --- Nyan Cat shows position in current buffer in mode-line.

;; Nyanyanyanyanyanyanya!

;; Author: Jacek "TeMPOraL" Zlydach <temporal.pl@gmail.com>
;; URL: https://github.com/TeMPOraL/nyan-mode/
;; Version: 1.1.1
;; Keywords: nyan, cat, lulz, scrolling, pop tart cat, build something amazing

;; This file is not part of GNU Emacs.

;; ...yet. ;).

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 3, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth
;; Floor, Boston, MA 02110-1301, USA.

;;; Commentary:

;; NEW! You can now click on the rainbow (or the empty space)
;; to scroll your buffer!

;; NEW! You can now customize the minimum window width
;; below which the nyan-mode will be disabled, so that more important
;; information can be shown in the modeline.

;; To activate, just load and put `(nyan-mode 1)' in your init file.

;; Contributions and feature requests welcome!

;; Inspired by (and in few places copied from) sml-modeline.el written by Lennart Borgman.
;; See: http://bazaar.launchpad.net/~nxhtml/nxhtml/main/annotate/head%3A/util/sml-modeline.el

;;; History:

;; 2016-04-26 - introduced click-to-scroll feature.

;; Started as a totally random idea back in August 2011.

;; The homepage at http://nyan-mode.buildsomethingamazing.com died somewhen in 2014/2015 because reasons.
;; I might get the domain back one day.

;;; Code:

(eval-when-compile (require 'cl))

(defconst +nyan-directory+ (file-name-directory (or load-file-name buffer-file-name)))

(defconst +nyan-cat-size+ 3)

(defconst +nyan-cat-image+ (concat +nyan-directory+ "img/nyan.xpm"))
(defconst +nyan-rainbow-image+ (concat +nyan-directory+ "img/rainbow.xpm"))
(defconst +nyan-outerspace-image+ (concat +nyan-directory+ "img/outerspace.xpm"))

(defconst +nyan-modeline-help-string+ "Nyanyanya!\nmouse-1: Scroll buffer position")

(defvar nyan-old-car-mode-line-position nil)

(defgroup nyan nil
  "Customization group for `nyan-mode'."
  :group 'frames)

(defun nyan-refresh ()
  "Refresh after option changes if loaded."
  (when (featurep 'nyan-mode)
    (when (and (boundp 'nyan-mode)
               nyan-mode)
      (nyan-mode -1)
      (nyan-mode 1))))

(defcustom nyan-animation-frame-interval 0.2
  "Number of seconds between animation frames."
  :type 'float
  :set (lambda (sym val)
         (set-default sym val)
         (nyan-refresh))
  :group 'nyan)

(defvar nyan-animation-timer nil)

(defun nyan-start-animation ()
  (interactive)
  (when (not (and nyan-animate-nyancat
		  nyan-animation-timer))
    (setq nyan-animation-timer (run-at-time "1 sec"
                                            nyan-animation-frame-interval
                                            'nyan-swich-anim-frame))
    (setq nyan-animate-nyancat t)))

(defun nyan-stop-animation ()
  (interactive)
  (when (and nyan-animate-nyancat
	     nyan-animation-timer)
    (cancel-timer nyan-animation-timer)
    (setq nyan-animation-timer nil)
    (setq nyan-animate-nyancat nil)))

(defcustom nyan-minimum-window-width 64
  "Determines the minimum width of the window, below which nyan-mode will not be displayed.
This is important because nyan-mode will push out all informations from small windows."
  :type 'integer
  :set (lambda (sym val)
         (set-default sym val)
         (nyan-refresh))
  :group 'nyan)

;;; FIXME bug, doesn't work for antoszka.
(defcustom nyan-wavy-trail nil
  "If enabled, Nyan Cat's rainbow trail will be wavy."
  :type '(choice (const :tag "Enabled" t)
                 (const :tag "Disabled" nil))
  :set (lambda (sym val)
         (set-default sym val)
         (nyan-refresh))
  :group 'nyan)

(defcustom nyan-bar-length 32
  "Length of Nyan Cat bar in units; each unit is equal to an 8px
  image. Minimum of 3 units are required for Nyan Cat."
  :type 'integer
  :set (lambda (sym val)
         (set-default sym val)
         (nyan-refresh))
  :group 'nyan)

(defcustom nyan-animate-nyancat nil
  "Enable animation for Nyan Cat.
This can be t or nil."
  :type '(choice (const :tag "Enabled" t)
                 (const :tag "Disabled" nil))
  :set (lambda (sym val)
         (set-default sym val)
         (if val
             (nyan-start-animation)
           (nyan-stop-animation))
         (nyan-refresh))
  :group 'nyan)

(defcustom nyan-cat-face-number 1
  "Select cat face number for console."
  :type 'integer
  :group 'nyan
  )

;;; Load images of Nyan Cat an it's rainbow.
(defvar nyan-cat-image (if (image-type-available-p 'xpm)
                           (create-image +nyan-cat-image+ 'xpm nil :ascent 'center)))

(defvar nyan-animation-frames (if (image-type-available-p 'xpm)
                                  (mapcar (lambda (id)
                                            (create-image (concat +nyan-directory+ (format "img/nyan-frame-%d.xpm" id))
                                                          'xpm nil :ascent 95))
                                          '(1 2 3 4 5 6))))
(defvar nyan-current-frame 0)

(defconst +nyan-catface+ [
                          ["[]*" "[]#"]
                          ["(*^ｰﾟ)" "( ^ｰ^)" "(^ｰ^ )" "(ﾟｰ^*)"]
                          ["(´ω｀三 )" "( ´ω三｀ )" "( ´三ω｀ )" "( 三´ω｀)"
                           "( 三´ω｀)" "( ´三ω｀ )" "( ´ω三｀ )" "(´ω｀三 )"]
                          ["(´д｀;)" "( ´д`;)" "( ;´д`)" "(;´д` )"]
                          ["(」・ω・)」" "(／・ω・)／" "(」・ω・)」" "(／・ω・)／"
                           "(」・ω・)」" "(／・ω・)／" "(」・ω・)」" "＼(・ω・)／"]
                          ["(＞ワ＜三　　　)" "(　＞ワ三＜　　)"
                           "(　　＞三ワ＜　)" "(　　　三＞ワ＜)"
                           "(　　＞三ワ＜　)" "(　＞ワ三＜　　)"]])

(defun nyan-toggle-wavy-trail ()
  "Toggle the trail to look more like the original Nyan Cat animation."
  (interactive)
  (setq nyan-wavy-trail (not nyan-wavy-trail)))

(defun nyan-swich-anim-frame ()
  (setq nyan-current-frame (% (+ 1 nyan-current-frame) 6))
  (redraw-modeline))

(defun nyan-get-anim-frame ()
  (if nyan-animate-nyancat
      (nth nyan-current-frame nyan-animation-frames)
    nyan-cat-image))

(defun nyan-wavy-rainbow-ascent (number)
  (if nyan-animate-nyancat
      (min 100 (+ 90
                  (* 3 (abs (- (/ 6 2)
                               (% (+ number nyan-current-frame)
                                  6))))))
    (if (zerop (% number 2)) 80 'center)))

(defun nyan-number-of-rainbows ()
  (round (/ (* (round (* 100
                         (/ (- (float (point))
                               (float (point-min)))
                            (float (point-max)))))
               (- nyan-bar-length +nyan-cat-size+))
            100)))

(defun nyan-catface () (aref +nyan-catface+ nyan-cat-face-number))

(defun nyan-catface-index ()
  (min (round (/ (* (round (* 100
                              (/ (- (float (point))
                                    (float (point-min)))
                                 (float (point-max)))))
                    (length (nyan-catface)))
                 100)) (- (length (nyan-catface)) 1)))

(defun nyan-scroll-buffer (percentage buffer)
  (interactive)
  (with-current-buffer buffer
    (goto-char (floor (* percentage (point-max))))))

(defun nyan-add-scroll-handler (string percentage buffer)
  (lexical-let ((percentage percentage)
                (buffer buffer))
    (propertize string 'keymap `(keymap (mode-line keymap (down-mouse-1 . ,(lambda () (interactive) (nyan-scroll-buffer percentage buffer))))))))

(defun nyan-create ()
  (if (< (window-width) nyan-minimum-window-width)
      ""                                ; disabled for too small windows
    (let* ((rainbows (nyan-number-of-rainbows))
           (outerspaces (- nyan-bar-length rainbows +nyan-cat-size+))
           (rainbow-string "")
           (xpm-support (image-type-available-p 'xpm))
           (nyancat-string (propertize
                            (aref (nyan-catface) (nyan-catface-index))
                            'display (nyan-get-anim-frame)))
           (outerspace-string "")
           (buffer (current-buffer)))
      (dotimes (number rainbows)
        (setq rainbow-string (concat rainbow-string
                                     (nyan-add-scroll-handler
                                      (if xpm-support
                                          (propertize "|"
                                                      'display (create-image +nyan-rainbow-image+ 'xpm nil :ascent (or (and nyan-wavy-trail
                                                                                                                            (nyan-wavy-rainbow-ascent number))
                                                                                                                       (if nyan-animate-nyancat 95 'center))))
                                        "|")
                                      (/ (float number) nyan-bar-length) buffer))))
      (dotimes (number outerspaces)
        (setq outerspace-string (concat outerspace-string
                                        (nyan-add-scroll-handler
                                         (if xpm-support
                                             (propertize "-"
                                                         'display (create-image +nyan-outerspace-image+ 'xpm nil :ascent (if nyan-animate-nyancat 95 'center)))
                                           "-")
                                         (/ (float (+ rainbows +nyan-cat-size+ number)) nyan-bar-length) buffer))))
      ;; Compute Nyan Cat string.
      (propertize (concat rainbow-string
                          nyancat-string
                          outerspace-string)
                  'help-echo +nyan-modeline-help-string+))))

;;;###autoload
(define-minor-mode nyan-mode
  "Use NyanCat to show buffer size and position in mode-line.
You can customize this minor mode, see option `nyan-mode'.
Note: If you turn this mode on then you probably want to turn off
option `scroll-bar-mode'."
  :global t
  :group 'nyan
  (if nyan-mode
      (progn
        (unless nyan-old-car-mode-line-position
          (setq nyan-old-car-mode-line-position (car mode-line-position)))
        (setcar mode-line-position '(:eval (list (nyan-create)))))
    (setcar mode-line-position nyan-old-car-mode-line-position)))


(provide 'nyan-mode)

;;; nyan-mode.el ends here
