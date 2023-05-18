;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

(setq user-full-name "Michael Reitmeir"
      user-mail-address "michi.reitmeir@gmail.com")

(setq-default
 delete-by-moving-to-trash t                      ; Delete files to trash
 window-combination-resize t                      ; take new window space from all other windows (not just current)
 x-stretch-cursor t)                              ; Stretch cursor to the glyph width

(setq undo-limit 80000000                         ; Raise undo-limit to 80Mb
      evil-want-fine-undo t                       ; By default while in insert all changes are one big blob. Be more granular
      auto-save-default t                         ; Nobody likes to loose work, I certainly don't
      truncate-string-ellipsis "…")

;; first, enter the new window
(setq evil-vsplit-window-right t
      evil-split-window-below t)
;; then, pull up buffer prompt
(defadvice! prompt-for-buffer (&rest _)
  :after '(evil-window-split evil-window-vsplit)
  (consult-buffer))

(setq doom-font (font-spec :family "JetBrains Mono" :size 15 :weight 'regular))
;;      doom-variable-pitch-font (font-spec :family "Fira Sans") ; inherits `doom-font''s :size
;;      doom-unicode-font (font-spec :family "Input Mono Narrow" :size 12)
;;      doom-big-font (font-spec :family "Fira Mono" :size 19))
(setq-default line-spacing 0.25)

;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. This is the default:

(setq doom-theme 'doom-one)

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
	   ))

(setq doom-modeline-height 35)

(after! doom-themes
	(custom-theme-set-faces! 'doom-one
	   `(doom-dashboard-banner :foreground "pink" :weight bold)
	   ))
(setq fancy-splash-image "~/.config/doom/I-am-doom.png")
(setq +doom-dashboard-banner-padding '(0 . 0))

(setq display-line-numbers-type 'visual)

(global-visual-line-mode t)

(defconst doom-frame-transparency 85)

(defun toggle-background-opacity ()
        "toggle transparent background"
        (interactive)
        (if (eq doom-frame-opacity 100)
            (setq doom-frame-opacity doom-frame-transparency)
            (setq doom-frame-opacity 100))
        (set-frame-parameter (selected-frame) 'alpha doom-frame-opacity)
        (add-to-list 'default-frame-alist `(alpha . ,doom-frame-opacity))
        (defun dwc-smart-transparent-frame ()
        (set-frame-parameter
        (selected-frame)
        'alpha (if (frame-parameter (selected-frame) 'fullscreen)
                100
                doom-frame-opacity))))

(setq doom-frame-opacity 100)
(toggle-background-opacity)

(use-package! whitespace
  :config (setq whitespace-style '(face empty indentation space-after-tab space-before-tab))
  (global-whitespace-mode +1))

(setq company-idle-delay 0.4)

(setq langtool-java-classpath "/usr/share/languagetool/*")

(add-hook 'snippet-mode-hook 'my-snippet-mode-hook)
(defun my-snippet-mode-hook ()
  "Custom behaviours for `snippet-mode'."
  (setq-local require-final-newline nil)
  (setq-local mode-require-final-newline nil))

; first unmap tab for snippets
(map! :map yas-minor-mode-map ; key for snippet expansion
      [tab] nil
      "TAB" nil)
(map! :map yas-keymap ; keys for navigation
      [tab] nil
      "TAB" nil
      [(shift tab)] nil
      [backtab] nil)

; then map pause for snippets instead
(map! :map 'yas-minor-mode-map ; key for snippet expansion
      [pause] #'yas-expand)
(map! :map yas-keymap ; keys for navigation
      [pause] 'yas-next-field-or-maybe-expand
      [(shift pause)] 'yas-prev)

(setq doom-localleader-key ",")

(setq wrapped-copy (symbol-function 'evil-delete))
(evil-define-operator evil-cut (BEG END TYPE REGISTER YANK-HANDLER)
  "Cut text from BEG to END with TYPE.

Save in REGISTER or in the kill-ring with YANK-HANDLER."
  (interactive "<R><x><y>")
  (funcall wrapped-copy BEG END TYPE REGISTER YANK-HANDLER))

(map! :n "m" 'evil-cut)

(defun bb/evil-delete (orig-fn beg end &optional type _ &rest args)
  (apply orig-fn beg end type ?_ args))
(advice-add 'evil-delete :around 'bb/evil-delete)
(advice-add 'evil-delete-char :around 'bb/evil-delete)

(defun bb/evil-org-delete-char (orig-fn count beg end &optional type _ &rest args)
  (apply orig-fn count beg end type ?_ args))
(advice-add 'evil-org-delete-char :around 'bb/evil-org-delete-char)

(map! :leader
 (:prefix ("t" . "toggle")
       :desc "transparency"          "t"     #'toggle-background-opacity
       )
      )

(map! :localleader
      :map org-mode-map
      (:prefix ("D" . "org-d20")
       :desc "start/advance combat" "i" #'org-d20-initiative-dwim
       :desc "add to combat" "a" #'org-d20-initiative-add
       :desc "apply damage at point" "d" #'org-d20-damage
       :desc "roll" "r" #'org-d20-roll
       )
      )

(setq org-directory "~/org/")
(setq org-agenda-files (list "~/org/todo.org" "~/org/lv_Sommer2023.org"))

(after! org
  (setq org-ellipsis " ▼ "
        org-superstar-headline-bullets-list '("◉" "●" "○" "◆" "●" "○" "◆")
        org-superstar-item-bullet-alist '((?+ . ?✦) (?- . ?➤)) ; changes +/- symbols in item lists
        org-log-done 'time
        org-agenda-skip-scheduled-if-done t     ; do not show scheduled items in agenda if they're already done
        org-agenda-skip-deadline-if-done t     ; do not show deadlines in agenda if they're already done
        org-deadline-warning-days 7
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

;;(setq +latex-viewers nil)
(setq +latex-indent-item-continuation-offset 'auto)
(setq evil-tex-toggle-override-m nil) ;; I want to use m for "move" (evil-cut)
;;... so I map toggle keybindings to localleader instead
(map! :localleader
      :map evil-tex-mode-map
      (:prefix ("t" . "toggle") ;; TODO this is not displaying descriptions properly, probably related to https://github.com/hlissner/doom-emacs/issues/4288
       :desc "command"          "c"     #'evil-tex-toggle-command
       :desc "delimiter"        "d"     #'evil-tex-toggle-delim
       :desc "environment"      "e"     #'evil-tex-toggle-env
       :desc "math"             "m"     #'evil-tex-toggle-math
       :desc "math align*"      "M"     #'evil-tex-toggle-math-align
       :desc "section"          "S"     #'evil-tex-toggle-section
       )
      )
;;
;; set maximum line length for visual-line-mode in tex-mode
(add-hook 'TeX-mode-hook 'window-margin-mode)
(setq-default fill-column 100)
