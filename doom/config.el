;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!


;; Some functionality uses this to identify you, e.g. GPG configuration, email
;; clients, file templates and snippets.
(setq user-full-name "Michael Reitmeir"
      user-mail-address "michi.reitmeir@gmail.com")

;; Doom exposes five (optional) variables for controlling fonts in Doom. Here
;; are the three important ones:
;;
;; + `doom-font'
;; + `doom-variable-pitch-font'
;; + `doom-big-font' -- used for `doom-big-font-mode'; use this for
;;   presentations or streaming.
;;
;; They all accept either a font-spec, font string ("Input Mono-12"), or xlfd
;; font string. You generally only need these two:
;; (setq doom-font (font-spec :family "monospace" :size 12 :weight 'semi-light)
;;       doom-variable-pitch-font (font-spec :family "sans" :size 13))

(setq doom-font (font-spec :family "JetBrains Mono" :size 15 :weight 'regular))
;;      doom-variable-pitch-font (font-spec :family "Fira Sans") ; inherits `doom-font''s :size
;;      doom-unicode-font (font-spec :family "Input Mono Narrow" :size 12)
;;      doom-big-font (font-spec :family "Fira Mono" :size 19))
(setq-default line-spacing 0.25)

;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. This is the default:
(setq doom-theme 'doom-one)

;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!
(setq org-directory "~/Dropbox/org/")

;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
(setq display-line-numbers-type 'visual)


;; Here are some additional functions/macros that could help you configure Doom:
;;
;; - `load!' for loading external *.el files relative to this one
;; - `use-package!' for configuring packages
;; - `after!' for running code after a package has loaded
;; - `add-load-path!' for adding directories to the `load-path', relative to
;;   this file. Emacs searches the `load-path' when you load packages with
;;   `require' or `use-package'.
;; - `map!' for binding new keys
;;
;; To get information about any of these functions/macros, move the cursor over
;; the highlighted symbol at press 'K' (non-evil users must press 'C-c c k').
;; This will open documentation for it, including demos of how they are used.
;;
;; You can also try 'gd' (or 'C-c c d') to jump to their definition and see how
;; they are implemented.


;; ----------------------------------------------------------------
;; ------------------ GLOBAL OPTIONS ----------------------------
;; always wrap overlength lines
(global-visual-line-mode t)
;; increase time until autocomplete shows up
(setq company-idle-delay 0.4)

;; ------------------ KEYBINDINGS ----------------------------
(setq doom-localleader-key ",")

;; clone the evil delete function and rename it evil-cut
(setq wrapped-copy (symbol-function 'evil-delete))
(evil-define-operator evil-cut (BEG END TYPE REGISTER YANK-HANDLER)
  "Cut text from BEG to END with TYPE.

Save in REGISTER or in the kill-ring with YANK-HANDLER."
  (interactive "<R><x><y>")
  (funcall wrapped-copy BEG END TYPE REGISTER YANK-HANDLER))
;; map cut to m (for move)
(evil-global-set-key 'normal "m" 'evil-cut)

;; automatically redirect all deletions to the black hole register
(defun bb/evil-delete (orig-fn beg end &optional type _ &rest args)
  (apply orig-fn beg end type ?_ args))
(advice-add 'evil-delete :around 'bb/evil-delete)
;; this way d and x and pasting over something all only delete and not cut

;; ------------------ TRANSPARENCY ----------------------------
(defconst doom-frame-transparency 85)
(set-frame-parameter (selected-frame) 'alpha doom-frame-transparency)
(add-to-list 'default-frame-alist `(alpha . ,doom-frame-transparency))
(defun dwc-smart-transparent-frame ()
  (set-frame-parameter
    (selected-frame)
    'alpha (if (frame-parameter (selected-frame) 'fullscreen)
              100
             doom-frame-transparency)))


;; ------------------ ORG CONFIG ----------------------------
(after! org
  (setq org-ellipsis " ▼ "
        org-superstar-headline-bullets-list '("◉" "●" "○" "◆" "●" "○" "◆")
        org-superstar-item-bullet-alist '((?+ . ?✦) (?- . ?➤)) ; changes +/- symbols in item lists
        org-log-done 'time
        org-agenda-skip-scheduled-if-done t     ; do not show scheduled items in agenda if they're already done
        org-todo-keywords        ; This overwrites the default Doom org-todo-keywords
          '((sequence
             "TODO(t)"
             "WAIT(w)"
             "TODELEGATE(T)"
             "IDEA(i)"
             "|"
             "DONE(d)"
             "DELEGATED(D)"
             "CANCELLED(c)" ))
	  org-todo-keyword-faces
	    '(("WAIT" . "#ECBE7B")
	      ("TODELEGATE" . "pink")
	      ("IDEA" . "cyan")
	      ("DONE" . "#5b8c68")
	      ("DELEGATED" . "#a9a1e1")
	      ("CANCELLED" . "#ff6c6b")
	     )
	  ))

(custom-set-faces!
  `(org-level-1 :inherit outline-1 :height 1.4)
  `(org-level-2 :inherit outline-2 :height 1.25)
  `(org-level-3 :inherit outline-3 :height 1.1)
  `(org-level-4 :inherit outline-4 :height 1.05)
  `(org-level-5 :inherit outline-5 :height 1.0)
  `(org-document-title :background nil :height 1.5 :weight bold)
)


;; ------------------ THEME CUSTOMIZATION ----------------------------
;; ------ WHITESPACE ------
(use-package! whitespace
  :config (setq whitespace-style '(face trailing empty indentation space-after-tab space-before-tab))
  (global-whitespace-mode +1))
;;
;;
(use-package ewal)
(use-package ewal-doom-themes)

(after! doom-themes
	(custom-theme-set-faces! 'doom-one
	   `(default :background ,(ewal-load-color 'background))
	   `(hl-line :background ,(ewal--color-chshade
				      (ewal-load-color 'background) .1))

	   ;; Tabs:
	   `(tab-bar :background ,(ewal-load-color 'background))
	   `(centaur-tabs-selected :background ,(ewal--color-chshade
				      (ewal-load-color 'background) .1))
	   `(tab-bar-tab :background ,(ewal--color-chshade
				      (ewal-load-color 'background) .1))
	   `(centaur-tabs-unselected :background ,(ewal--color-chshade
				      (ewal-load-color 'background) .05))
	   `(tab-bar-tab-inactive :background ,(ewal--color-chshade
				      (ewal-load-color 'background) .05))
	   `(tab-line :background ,(ewal-load-color 'background))

	   ;; Mode line:
	   `(mode-line :background ,(ewal--color-chshade
				      (ewal-load-color 'background) .15))
	   `(mode-line-inactive :background ,(ewal--color-chshade
				      (ewal-load-color 'background) .05))
	   `(mode-line-emphasis :background ,(ewal--color-chshade
				      (ewal-load-color 'background) .20))

	   ;; minibuffer (underneath mode line) and stuff
	   `(solaire-default-face :background ,(ewal-load-color 'background))

	   ;; Dashboard (startup page)
	   `(doom-dashboard-banner :foreground "pink" :weight bold)
	   ))
(setq fancy-splash-image "~/.config/doom/I-am-doom.png") ;; https://github.com/jeetelongname/doom-banners
(setq +doom-dashboard-banner-padding '(0 . 0)) ;; remove whitespace after splash


;; -------- CREATE CONTAINING HEADINGS WHEN ARCHIVING
;; stolen from https://gist.github.com/edgimar/072d99d8650abe81a9fe7c8687c0c993
;; small fix from Lukas Barth: https://emacs.stackexchange.com/questions/47660/org-mode-archiving-create-containing-headings
;; customized variables and keybinds by me
(require 'org-archive)

; Set the function to use for org-archive-default  (C-c C-x C-a)
(setq org-archive-location "archive.org::")

; unmap org-archive-subtree
(define-key org-mode-map (kbd "C-c C-x C-s") nil)

; select command to execute via org-archive-subtree-default (C-c C-x C-a)
(setq org-archive-default-command 'org-archive-subtree-hierarchical)
;; overwrite default DOOM archive mapping
(map! :map org-mode-map
      :localleader
      :desc "archive subtree" "A" 'org-archive-subtree-hierarchical)

(defun line-content-as-string ()
  "Returns the content of the current line as a string"
  (save-excursion
    (beginning-of-line)
    (buffer-substring-no-properties
     (line-beginning-position) (line-end-position))))

(defun org-child-list (&optional top-level)
  "This function returns all children of a heading as a list. "
  (interactive)
  (save-excursion
    ;; this only works with org-version > 8.0, since in previous
    ;; org-mode versions the function (org-outline-level) returns
    ;; gargabe when the point is not on a heading.
    (unless top-level
        (if (= (org-outline-level) 0)
            (outline-next-visible-heading 1)
        (org-goto-first-child)))
    (let ((child-list (list (line-content-as-string))))
      (while (org-goto-sibling)
        (setq child-list (cons (line-content-as-string) child-list)))
      child-list)))

(defun fa/org-struct-subtree ()
  "This function returns the tree structure in which a subtree belongs as a list."
  (interactive)
  (let ((archive-tree nil))
    (save-excursion
      (while (org-up-heading-safe)
        (let ((heading
               (buffer-substring-no-properties
                (line-beginning-position) (line-end-position))))
          (if (eq archive-tree nil)
              (setq archive-tree (list heading))
            (setq archive-tree (cons heading archive-tree))))))
    archive-tree))

(defun org-archive-subtree-hierarchical ()
  "This function archives a subtree hierarchical"
  (interactive)
  (let ((org-tree (fa/org-struct-subtree))
        (source-buffer (current-buffer))
        (file (abbreviate-file-name
                   (or (buffer-file-name (buffer-base-buffer))
                       (error "No file associated to buffer")))))
    (save-excursion
      (setq location (org-archive--compute-location
                (or (org-entry-get nil "ARCHIVE" 'inherit)
                    org-archive-location))
            afile (car location)
            heading (cdr location)
            infile-p (equal file (abbreviate-file-name (or afile ""))))
      (unless afile
        (error "Invalid `org-archive-location'"))
      (if (not (equal heading ""))
          (progn
            (setq org-tree (cons heading
                               (mapcar (lambda (s) (concat "*" s)) org-tree)))
            (org-demote-subtree)))
      (if (> (length afile) 0)
        (progn
          (setq newfile-p (not (file-exists-p afile))
                visiting (find-buffer-visiting afile)
                target-buffer (or visiting (find-file-noselect afile))))
        (progn
          (setq target-buffer (current-buffer))))
      (unless target-buffer
        (error "Cannot access file \"%s\"" afile))
      (org-cut-subtree)
      (set-buffer target-buffer)
      (setq ind-target-buffer (clone-indirect-buffer nil nil))
      (set-buffer ind-target-buffer)
      (org-mode)
      (goto-char (point-min))

      ; simplified version of org-complex-heading-regexp-format
	  (setq my-org-complex-heading-regexp-format
	      (concat "^"
		      "\\(%s\\)"
		      "\\(?: *\\[[0-9%%/]+\\]\\)*"
		      "\\(?:[ \t]+\\(:[[:alnum:]_@#%%:]+:\\)\\)?"
		      "[ \t]*$"))
      (setq top-level-p t)
      (while (not (equal org-tree nil))
        (let ((child-list (org-child-list top-level-p))
              (re (format my-org-complex-heading-regexp-format (regexp-quote (car org-tree))))
             )
          (if (member "______FOUND_MATCH" (mapcar (lambda (s) (replace-regexp-in-string re "______FOUND_MATCH" s)) child-list))
              (progn
                (re-search-forward re nil t)
                (setq org-tree (cdr org-tree)))
            (progn
              (if (not top-level-p) (newline))
              (org-insert-struct org-tree)
              (setq org-tree nil))))
        (setq top-level-p nil))
      (newline)
      (org-yank)
      ;; Kill the indirect buffer, returning the current buffer to the direct target buffer
      (kill-buffer ind-target-buffer)
      ;; Save and kill the target buffer, if it is not the source buffer.
      (when (not (eq source-buffer target-buffer))
        (when (not (eq source-buffer target-buffer)) (with-current-buffer target-buffer (save-buffer) ) ) ;; this is Lukas' fix
        (kill-buffer target-buffer))
      ;; ensure font-lock and indentation are normal
      (set-buffer source-buffer)
      (org-restart-font-lock)
      (org-indent-mode t)
      (message "Subtree archived %s"
               (concat "in file: " (abbreviate-file-name afile))))))

(defun org-insert-struct (struct)
  "TODO"
  (interactive)
  (when struct
    (insert (car struct))
    (if  (not (equal (length struct) 1))
        (newline))
    (org-insert-struct (cdr struct))))
