;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; PARAMETERS AND VARIABLES ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar my-sch-directory "~/Documents/AY3S1/")

;;;;;;;;;;;;;;;;;;;;;;;;;;
;; FUNCTION DEFINITIONS ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;; COPY AND PASTE

(defun copy-to-clipboard ()
  "Copies selection to clipboard."
  (interactive)
  (if (display-graphic-p)
      (progn
        (message "Yanked to clipboard.")
        (call-interactively 'clipboard-kill-ring-save))
    (if (region-active-p)
        (progn
          (shell-command-on-region (region-beginning) (region-end) "xsel -i -b")
          (message "Yanked to clipboard.")
          (deactivate-mark))
      (message "No region active; can't yank to clipboard."))))

(defun cut-to-clipboard ()
  "Cuts selection to clipboard."
  (interactive)
  (if (display-graphic-p)
      (progn
        (message "Cut to clipboard.")
        (call-interactively 'clipboard-kill-region))
    (if (region-active-p)
        (progn
          (shell-command-on-region (region-beginning) (region-end) "xsel -i -b")
          (message "Cut to clipboard.")
          (evil-delete))
      (message "No region active; can't yank to clipboard."))))

(defun paste-from-clipboard ()
  "Pastes from clipboard."
  (interactive)
  (if (display-graphic-p)
      (progn
        (clipboard-yank)
        (message "Pasted from clipboard."))
    (insert (shell-command-to-string "xsel -o -b"))))

;;;; CUSTOM ORG FILE TEMPLATES

(defvar sch-org-file-template ;; supply <title>
"#+TITLE: %s
#+LATEX_HEADER: \\usepackage{indentfirst}
#+LATEX_HEADER: \\usepackage{parskip}  \\setlength{\\parindent}{15pt}
#+LATEX_HEADER: \\usepackage{sectsty}  \\setcounter{secnumdepth}{2}
#+LATEX_HEADER: \\usepackage{titlesec} \\newcommand{\\sectionbreak}{\\clearpage}
#+LATEX_HEADER: \\usepackage[margin=0.5in]{geometry}
#+LATEX_HEADER: \\usepackage[outputdir=Output]{minted}
#+OPTIONS: toc:2 author:nil date:nil\n")

(defvar sch-org-cheatsheet-template ;; supply <title> <title>
"#+TITLE: %s
#+LATEX_CLASS: article
#+LATEX_HEADER: \\usepackage{parskip}  \\setlength{\\parindent}{0pt} \\setlength{\\parskip}{2pt}
#+LATEX_HEADER: \\usepackage{sectsty} \\setcounter{secnumdepth}{1} \\allsectionsfont{\\raggedright}
#+LATEX_HEADER: \\usepackage{enumitem} \\setlist[1]{itemsep=-2pt} \\setlist[itemize]{leftmargin=*} \\setlist[enumerate]{leftmargin=*}
#+LATEX_HEADER: \\usepackage{titlesec} \\titleformat{\\section}{\\large\\bfseries\\raggedright}{\\thesection.}{\\hspace{5pt}}{} \\titleformat*{\\subsection}{\\footnotesize\\bfseries\\raggedright} \\titlespacing{\\section}{0pt}{6pt}{2pt} \\titlespacing{\\subsection}{0pt}{4pt}{0pt}
#+LATEX_HEADER: \\usepackage[a4paper, landscape, margin=0.3in]{geometry}
#+LATEX_HEADER: \\usepackage{multicol}
#+LATEX_HEADER: \\usepackage[outputdir=Output]{minted}
#+OPTIONS: author:nil title:nil toc:nil date:nil

\\centering
\\header{%s}

\\raggedright
\\begin{multicols*}{4}
\\footnotesize

<insert text here>

\\end{multicols*}\n")

(defvar sch-org-tutorial-template ;; supply <title>
"#+TITLE: %s
#+AUTHOR: <fill me in>
#+LATEX_HEADER: \\usepackage{indentfirst}
#+LATEX_HEADER: \\usepackage{parskip}  \\setlength{\\parindent}{15pt}
#+LATEX_HEADER: \\usepackage{sectsty}  \\setcounter{secnumdepth}{2}
#+LATEX_HEADER: \\usepackage[margin=0.5in]{geometry}
#+OPTIONS: toc:nil date:nil num:nil\n")

(defun zw/create-sch-org-file (module-code fname title)
  "Creates an org file with appropriate metadata in the variable
   <my-sch-directory>. Prompts for title."
  (interactive "sModule code? \nsFilename? \nsTitle? ")
  (let* ((date-string (format-time-string "[%Y-%m-%d]"))
         (file-name fname)
         (destination (format "%s%s/%s.org" my-sch-directory module-code file-name))
         (formatted-head (format sch-org-file-template title)))
    (write-region formatted-head nil destination)
    (switch-to-buffer (find-file-noselect destination))
    (goto-char (point-max))))

(defun zw/create-sch-org-cheatsheet (module-code fname title)
  "Creates an org cheatsheet with appropriate metadata in the variable
   <my-sch-directory>. Prompts for title."
  (interactive "sModule code? \nsFilename? \nsTitle? ")
  (let* ((file-name fname)
         (destination (format "%s%s/%s.org" my-sch-directory module-code file-name))
         (formatted-head (format sch-org-cheatsheet-template title title)))
    (write-region formatted-head nil destination)
    (switch-to-buffer (find-file-noselect destination))
    (goto-char (point-max))))

(defun zw/create-sch-org-tutorial (module-code fname title)
  "Creates an org tutorial with appropriate metadata in the variable
   <my-sch-directory>. Prompts for title."
  (interactive "sModule code? \nsFilename? \nsTitle? ")
  (let* ((file-name fname)
         (destination (format "%s%s/%s.org" my-sch-directory module-code file-name))
         (formatted-head (format sch-org-tutorial-template title)))
    (write-region formatted-head nil destination)
    (switch-to-buffer (find-file-noselect destination))
    (goto-char (point-max))))

;;;; TREE VIEW

(defun open-tree-view ()
  "Open a clone of the current buffer to the left, resize it to 40 columns, and bind <mouse-1> to jump to the same position in the base buffer."
  (interactive)
  (let ((new-buffer-name (concat "*<tree>" (buffer-name) "*")))
    ;; Create tree buffer
    (split-window-right 40)
    (if (get-buffer new-buffer-name)
        (switch-to-buffer new-buffer-name)  ; Use existing tree buffer
      ;; Make new tree buffer
      (progn  (clone-indirect-buffer new-buffer-name nil t)
              (switch-to-buffer new-buffer-name)
              (read-only-mode)
              (hide-body)
              (toggle-truncate-lines)
              (org-content)
              (evil-digit-argument-or-evil-beginning-of-line)
              (spacemacs/scale-down-font) (spacemacs/scale-down-font)

              ;; Do this twice in case the point is in a hidden line
              (dotimes (_ 2 (forward-line 0)))

              ;; Map keys
              (use-local-map (copy-keymap outline-mode-map))
              (local-set-key (kbd "<backtab>") 'org-content)
              (local-set-key (kbd "<tab>") 'jump-to-point-and-show)
              (local-set-key (kbd "<mouse-1>") 'jump-to-point-and-show)))))

(defun jump-to-point-and-show ()
  "Switch to a cloned buffer's base buffer and move point to the same cursor position in the clone. If in visual, exit visual."
  (interactive)
  (evil-exit-visual-state)
  (let ((buf (buffer-base-buffer)))
    (unless buf
      (error "You need to be in a cloned buffer!"))
    (let ((pos (point))
          (win (car (get-buffer-window-list buf))))
      (if win
          (select-window win)
        (other-window 1)
        (switch-to-buffer buf))
      (goto-char pos)
      (when (invisible-p (point))
        (show-branches))
      (evil-scroll-line-to-top (line-number-at-pos (point))))))

;;;; MISCELLANEOUS

(defun browse-file-directory ()
  "Opens the directory containing the current file."
  (interactive)
  (if default-directory
      (browse-url-of-file (expand-file-name default-directory))))

;;;;;;;;;;;;;;;;;;;;;;;;
;; KEYBOARD SHORTCUTS ;;
;;;;;;;;;;;;;;;;;;;;;;;;

;; Enable <SPC o y> and <SPC o p> for cutting, copying to, and pasting from the clipboard respectively
(map! :leader :desc "Copy to clipboard" "o y" 'copy-to-clipboard)
(map! :leader :desc "Cut to clipboard" "o x" 'cut-to-clipboard)
(map! :leader :desc "Paste from clipboard" "o p" 'paste-from-clipboard)

;; Enable <SPC o c> for opening default directory of current buffer
(map! :leader :desc "Open file directory" "o c" 'browse-file-directory)

;; Enable <SPC o n> for disabling search highlighting in vim
(map! :leader :desc "Disable search highlighting" "o n" 'evil-ex-nohighlight)

;; Enable <SPC o t> for opening tree view for org mode
(map! :leader :desc "Open tree view" "o t" 'open-tree-view)

;; Make up/down operate in screen lines instead of logical lines, in both normal and visual mode
(map! :nv "j" 'evil-next-visual-line)
(map! :nv "k" 'evil-previous-visual-line)

;; Have extra keybindings for winner undo and redo
(map! "C-c [" 'winner-undo)
(map! "C-c ]" 'winner-redo)

;; Enable <h>/<l> for left/right movement shortcuts in DocView
(add-hook 'doc-view-mode-hook
          '(lambda ()
             (map! :map evil-motion-state-local-map "h" 'doc-view-previous-page)
             (map! :map evil-motion-state-local-map "l" 'doc-view-next-page)))

;; Enable <SPC TAB <> and <SPC TAB >> for shifting left/right of workspaces
(map! :leader "TAB <" '+workspace/swap-left)
(map! :leader "TAB >" '+workspace/swap-right)
