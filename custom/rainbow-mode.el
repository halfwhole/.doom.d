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

(defcustom nyan-minimum-window-width 64
  "Determines the minimum width of the window, below which nyan-mode will not be displayed.
This is important because nyan-mode will push out all informations from small windows."
  :type 'integer
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

(defun nyan-number-of-rainbows ()
  (round (/ (* (round (* 100
                         (/ (- (float (point))
                               (float (point-min)))
                            (float (point-max)))))
               nyan-bar-length)
            100)))

(defun nyan-scroll-buffer (percentage buffer)
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
           (outerspaces (- nyan-bar-length rainbows))
           (rainbow-string "")
           (xpm-support (image-type-available-p 'xpm))
           (outerspace-string "")
           (buffer (current-buffer)))
      (dotimes (number rainbows)
        (setq rainbow-string (concat rainbow-string
                                     (nyan-add-scroll-handler
                                      (if xpm-support
                                          (propertize "|"
                                                      'display (create-image +nyan-rainbow-image+ 'xpm nil :ascent 'center))
                                        "|")
                                      (/ (float number) nyan-bar-length) buffer))))
      (dotimes (number outerspaces)
        (setq outerspace-string (concat outerspace-string
                                        (nyan-add-scroll-handler
                                         (if xpm-support
                                             (propertize "-"
                                                         'display (create-image +nyan-outerspace-image+ 'xpm nil :ascent 'center))
                                           "-")
                                         (/ (float (+ rainbows number)) nyan-bar-length) buffer))))
      ;; Compute Nyan Cat string.
      (propertize (concat rainbow-string outerspace-string) 'help-echo +nyan-modeline-help-string+))))

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
