(define-minor-mode zorg-mode
  "My custom minor mode for Org-mode."
  :lighter " Zorg"
  :keymap ()

  (setq line-spacing 0.1)
  (setq right-margin-width 1)

  (setq-local adaptive-wrap-prefix-mode 0)       ;; Disables adaptive-wrap so that org-indent-mode can do its thing
  ;; NOTE: I have disabled adaptive-wrap-prefix-mode globally (I think).
  ;; To re-enable, go to ~./.emacs.d/layers/+spacemacs/spacemacs-editing-visual/packages.el.

  (setq org-pretty-entities t)        ;; Pretty-entities allows the use of \ast{} for *, \plus{} for +, \under{} for _, etc.
  (setq org-hide-emphasis-markers t)  ;; Hides text formatting (e.g. bold, italics, underline)
  (setq org-emphasis-alist '(("*" bold)
                             ("/" italic)
                             ("_" underline)
							 ("+" (:strike-through t))
                             ("=" org-verbatim verbatim)
                             ("~" region)))

  ;; Sets font and parameters for the headlines
  (let* ((variable-tuple (cond ((x-list-fonts "Source Sans Pro") '(:font "Source Sans Pro"))
                               ((x-list-fonts "Verdana")         '(:font "Verdana"))
                               ((x-family-fonts "Sans Serif")    '(:family "Sans Serif"))
                               (nil (warn "Cannot find a Sans Serif Font.  Install Source Sans Pro."))))
         (base-font-color     (face-foreground 'default nil 'default))
         (headline           `(:inherit default :foreground ,base-font-color)))
    (custom-theme-set-faces 'user
                            `(org-level-8 ((t (,@headline ,@variable-tuple))))
                            `(org-level-7 ((t (,@headline ,@variable-tuple))))
                            `(org-level-6 ((t (,@headline ,@variable-tuple))))
                            `(org-level-5 ((t (,@headline ,@variable-tuple))))
                            `(org-level-4 ((t (,@headline ,@variable-tuple :height 1.1  :foreground "#b1951d"))))
                            `(org-level-3 ((t (,@headline ,@variable-tuple :height 1.25 :foreground "#67b11d"))))
                            `(org-level-2 ((t (,@headline ,@variable-tuple :height 1.5  :foreground "#2d9574"))))
                            `(org-level-1 ((t (,@headline ,@variable-tuple :height 1.75 :foreground "#4f97d7"))))
                            `(org-document-title ((t (,@headline ,@variable-tuple :height 1.25 :underline nil))))
							`(org-document-info  ((t (,@headline ,@variable-tuple))))))

  ;; (require 'ox-latex)
  ;; (add-to-list 'org-latex-packages-alist '("" "minted"))
  ;; (setq org-latex-listings 'minted
  ;;       org-latex-pdf-process
  ;;       '("pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f"
  ;;         "pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f"
  ;;         "pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f"))
  )

(provide 'zorg-mode)

