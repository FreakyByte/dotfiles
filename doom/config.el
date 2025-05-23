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
      truncate-string-ellipsis "…"
      completion-ignore-case t)

(setq evil-vsplit-window-right t
      evil-split-window-below t)

(defconst doom-module-config-file (concat doom-user-dir "config.org"))
(defun open-emacs-config-file () "open `doom-module-config-file'"
  (interactive)
  (find-file doom-module-config-file))
(map! :leader
 (:prefix ("o" . "open")
       :desc "Emacs config"     "c"     #'open-emacs-config-file))

(setq shell-file-name (executable-find "bash"))

(setq emacs-everywhere-frame-name-format "emacs-everywhere")

(remove-hook 'emacs-everywhere-init-hooks #'emacs-everywhere-set-frame-position)

(setq emacs-everywhere-major-mode-function #'org-mode)
(add-hook 'emacs-everywhere-init-hooks 'org-mode)

(setq doom-font (font-spec :family "JetBrains Mono" :size 15 :weight 'regular)
      doom-variable-pitch-font (font-spec :family "Libertinus Sans" :size 19))
;;      doom-variable-pitch-font (font-spec :family "Fira Sans") ; inherits `doom-font''s :size
;;      doom-unicode-font (font-spec :family "Input Mono Narrow" :size 12)
;;      doom-big-font (font-spec :family "Fira Mono" :size 19))

(setq-default line-spacing 0.25)

(setq mixed-pitch-set-height t)

(map! :leader
 (:prefix ("t" . "toggle")
       :desc "Mixed pitch mode"       "m"     #'mixed-pitch-mode
       :desc "Variable pitch mode"    "v"     #'variable-pitch-mode
       )
      )

(map! :leader
 (:prefix ("t" . "toggle")
       :desc "Visible mode"           "V"     #'visible-mode
       )
      )

(setq doom-theme 'doom-one)

(use-package ewal)
(use-package ewal-doom-themes)

(defvar ewal-background-list
  '((default :background (ewal-load-color 'background))
    (separator-line :background (ewal-load-color 'background))
    (hl-line :background (ewal--color-chshade (ewal-load-color 'background) .1))
    (org-block :background (ewal--color-chshade (ewal-load-color 'background) -0.3))
        ;; Tabs:
    (tab-bar :background (ewal-load-color 'background))
    (tab-bar-tab :background (ewal--color-chshade (ewal-load-color 'background) .1))
    (tab-bar-tab-inactive :background (ewal--color-chshade (ewal-load-color 'background) .05))
    (tab-line :background (ewal-load-color 'background))
        ;; Mode line:
    (mode-line :background (ewal--color-chshade (ewal-load-color 'background) .15))
    (mode-line-inactive :background (ewal--color-chshade (ewal-load-color 'background) .05))
    (mode-line-emphasis :background (ewal--color-chshade (ewal-load-color 'background) .20))
        ;; minibuffer (underneath mode line) and stuff
    (solaire-default-face :background (ewal-load-color 'background)))
  "list of faces to customize when styling emacs with ewal")

(defvar ewal-background-active nil "non-nil if background is currently styled using ewal")

(defun toggle-ewal-background ()
  "toggle ewal background colors on and off"
  (interactive)
  (if ewal-background-active
        (doom/reload-theme)
        (dolist (spec ewal-background-list)
                (let ((face (nth 0 spec))
                (attribute (nth 1 spec))
                (value (nth 2 spec)))
                (set-face-attribute face nil attribute (eval value)))))
  (setq ewal-background-active (not ewal-background-active)))

(map! :leader
 (:prefix ("t" . "toggle")
       :desc "background colors"        "B"     #'toggle-ewal-background))

(after! (doom-themes org hl-line)
        (toggle-ewal-background))

(setq doom-modeline-height 35)

(after! doom-themes
    (custom-theme-set-faces! 'doom-one
        `(doom-dashboard-banner :foreground "pink" :weight bold)))
(setq fancy-splash-image "~/.config/doom/I-am-doom.png")
(setq +doom-dashboard-banner-padding '(0 . 4))

(setq +doom-dashboard-functions
      '(doom-dashboard-widget-banner
        doom-dashboard-widget-shortmenu
        doom-dashboard-widget-projects))

(setq +doom-dashboard-menu-sections
 '(("Configure Emacs"
    :icon (concat (nerd-icons-icon-for-mode 'emacs-lisp-mode :face 'doom-dashboard-menu-title) " ")
    :action open-emacs-config-file
    :key "SPC o c")
   ("Take some notes"
    :icon (concat (nerd-icons-faicon "nf-fa-file_pen" :face 'doom-dashboard-menu-title) " ")
    :action org-roam-node-find-default
    :key "SPC r f")
   ("Read some literature"
    :icon (concat (nerd-icons-faicon "nf-fa-book" :face 'doom-dashboard-menu-title) " ")
    :action citar-open-files
    :key "SPC l f")
   ("Work on a project"
    :icon (concat (nerd-icons-faicon "nf-fa-code" :face 'doom-dashboard-menu-title) " ")
    :action projectile-switch-project
    :key "SPC p p")))

(require 'projectile)
(defface doom-dashboard-project
  '((t :inherit font-lock-string-face))
  "Face for projects on doom-dashboard.")
(defun doom-dashboard-widget-projects ()
  "Create list of menu buttons for each project in `projectile-known-projects', and bind key appropriately."
        (projectile-known-projects)
        (dolist (project projectile-known-projects)
         (insert
          (+doom-dashboard--center (- +doom-dashboard--width 1)
           ; put together three things:
           (format "%s %-35s%-6s\n"
            ; 1. the icons
            (nerd-icons-faicon "nf-fa-angle_right" :face 'doom-dashboard-project)
            ; 2. the project button
            (with-temp-buffer
             (insert-text-button project
                        'action `(lambda (_button) (let
                                ((projectile-switch-project-action 'magit-status))
                                (projectile-switch-project-by-name ,project)))
                        'face 'doom-dashboard-project
                        'follow-link t
                        'help-echo project)
             (buffer-string))
            ; 3. the number (with face and keybindings)
            (let* ((num (+ 1 (cl-position project projectile-known-projects)))
                  (str (number-to-string num)))
             (map! :map +doom-dashboard-mode-map :ng str
                   `(lambda () (interactive) (let
                                ((projectile-switch-project-action 'magit-status))
                                (projectile-switch-project-by-name ,project))))
             (add-text-properties 0 (length str) (list 'face 'doom-dashboard-menu-desc) str)
              str))))))
(after! (projectile doom)
    (setq! +workspaces-switch-project-function #'doom-project-browse)
    (add-to-list 'projectile-ignored-projects "~/.config/emacs/"))

(defadvice! no-new-lines (oldfun)
  :around #'doom-dashboard-widget-shortmenu
  (cl-letf (((symbol-function 'display-graphic-p) #'ignore))
    (funcall oldfun)))

(map! :map +doom-dashboard-mode-map
      :ng "c"       #'open-emacs-config-file
      :ng "r"       #'org-roam-node-find-default
      :ng "l"       #'citar-open-files
      :ng "p"       #'projectile-switch-project)

(setq display-line-numbers-type 'visual)

(global-visual-line-mode t)

(setq-default fill-column 110)

(use-package! perfect-margin
  :config
  (after! doom-modeline
    (setq mode-line-right-align-edge 'right-fringe))
  (after! minimap
    ;; if you use (vc-gutter +pretty)
    ;; and theme is causing "Invalid face attribute :foreground nil"
    ;; (setq minimap-highlight-line nil)
    (setq minimap-width-fraction 0.08))
  ;; (setq perfect-margin-only-set-left-margin t)
  (perfect-margin-mode t)
  ;; make perfect-margin use fill-column as width
  (setq perfect-margin-visible-width -1))
(map! :leader
 (:prefix ("t" . "toggle")
       :desc "Perfect margin mode"  "p"     #'perfect-margin-mode))

(add-to-list 'perfect-margin-ignore-filters '(lambda (window) (bound-and-true-p writeroom-mode)))
(add-to-list 'perfect-margin-ignore-filters '(lambda (window) (bound-and-true-p doom-big-font-mode)))

(defadvice doom-big-font-mode (before deactivate-perfect-margins) (perfect-margin-mode 0))

(add-hook 'doom-docs-mode-hook
  (lambda ()
    (setq-local ignored-local-variables (push 'fill-column ignored-local-variables))
    (setq-local fill-column 90)))

(setq writeroom-width 45)
(map! :leader
 (:prefix ("t" . "toggle")
       :desc "Global writeroom mode"  "W"     #'global-writeroom-mode))

(defconst frame-default-opacity 90)

(defvar opacity-type "full-frame" "Type of opacity to use. If set to \"background\" only the background will be transparent. If set to \"full-frame\", the entire frame will be transparent. Needs to be refreshed using `update-background-opacity'")
(defun update-background-opacity ()
        "update transparency to the value of `frame-opacity' and the type `opacity-type'"
        (interactive)
        (cond
         ((equal opacity-type "background")
                (set-frame-parameter (selected-frame) 'alpha-background frame-opacity)
                (set-frame-parameter (selected-frame) 'alpha 100))
          ((equal opacity-type "full-frame")
                (set-frame-parameter (selected-frame) 'alpha-background 100)
                (set-frame-parameter (selected-frame) 'alpha frame-opacity))))
(add-hook! 'doom-switch-frame-hook :append #'update-background-opacity)

(defun toggle-frame-opacity ()
        "toggle opacity of the frame"
        (interactive)
        (if (= frame-opacity 100)
            (setq frame-opacity frame-default-opacity)
            (setq frame-opacity 100))
        (update-background-opacity))
(defun toggle-opacity-type ()
        "toggle between transparent background and fully transparent frame"
        (interactive)
        (if (equal opacity-type "background")
            (setq opacity-type "full-frame")
            (setq opacity-type "background"))
        (update-background-opacity))

(map! :leader
 (:prefix ("t" . "toggle")
       :desc "transparent background"          "t"     #'toggle-frame-opacity
       :desc "transparency type"               "T"     #'toggle-opacity-type))

(setq frame-opacity 100)
(toggle-frame-opacity)

(use-package! whitespace
  :config (setq whitespace-style '(face empty indentation space-after-tab space-before-tab))
  (global-whitespace-mode +1))

(setq treemacs-width 30)
(setq treemacs--width-is-locked nil)
(setq treemacs-width-is-initially-locked nil)

(after! treemacs
  (defvar treemacs-file-ignore-extensions '()
    "File extension which `treemacs-ignore-filter' will ensure are ignored")
  (defvar treemacs-file-ignore-globs '()
    "Globs which will are transformed to `treemacs-file-ignore-regexps' which `treemacs-ignore-filter' will ensure are ignored")
  (defvar treemacs-file-ignore-regexps '()
    "RegExps to be tested to ignore files, generated from `treeemacs-file-ignore-globs'")
  (defun treemacs-file-ignore-generate-regexps ()
    "Generate `treemacs-file-ignore-regexps' from `treemacs-file-ignore-globs'"
    (setq treemacs-file-ignore-regexps (mapcar 'dired-glob-regexp treemacs-file-ignore-globs)))
  (if (equal treemacs-file-ignore-globs '()) nil (treemacs-file-ignore-generate-regexps))
  (defun treemacs-ignore-filter (file full-path)
    "Ignore files specified by `treemacs-file-ignore-extensions', and `treemacs-file-ignore-regexps'"
    (or (member (file-name-extension file) treemacs-file-ignore-extensions)
        (let ((ignore-file nil))
          (dolist (regexp treemacs-file-ignore-regexps ignore-file)
            (setq ignore-file (or ignore-file (if (string-match-p regexp full-path) t nil)))))))
  (add-to-list 'treemacs-ignored-file-predicates #'treemacs-ignore-filter))

(setq treemacs-file-ignore-extensions
      '(;; LaTeX
        "aux"
        "ptc"
        "fdb_latexmk"
        "fls"
        "synctex.gz"
        "gz" ; the function actually recognizes the last '.', not the first; I don't think I'll ever need to look at .gz-files anyways
        "toc"
        ;; LaTeX - glossary
        "glg"
        "glo"
        "gls"
        "glsdefs"
        "ist"
        "acn"
        "acr"
        "alg"
        ;; LaTeX - pgfplots
        "mw"
        ;; LaTeX - pdfx
        "pdfa.xmpi"
        ;; further LaTeX stuff
        "bbl"
        "bcf"
        "blg"
        "nav"
        "out"
        "snm"
        "vrb"
        "tdl"
        ))
(setq treemacs-file-ignore-globs
      '(;; LaTeX
        "*/_minted-*"
        ;; AucTeX
        "*/.auctex-auto"
        "*/_region_.log"
        "*/_region_.tex"))

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

(map! :n   "ö"   'evil-next-buffer
      :n   "Ö"   'evil-prev-buffer
      :nig "C-ö" 'switch-to-buffer
      :nig "C-j" 'evil-window-next
      :nig "C-k" 'evil-window-prev
      :nig "C-l" 'evil-window-vsplit
      :nig "C-ä" 'evil-window-split)
(map! :after org
    :map org-mode-map
    "C-j" 'evil-window-next)

(defadvice! prompt-for-buffer (&rest _)
  :after '(evil-window-split evil-window-vsplit)
  (if (<= (length (persp-buffer-list)) 1) nil (call-interactively 'persp-switch-to-buffer)))

(setq company-idle-delay 0.4)

(defun advice--replace-whole-buffer (oldfun &rest args)
        "advice for search functions to search the whole buffer (if not specified otherwise)"
        ;; set start pos
        (unless (nth 3 args)
                (setf (nth 3 args)
                (if (region-active-p)
                        (region-beginning)
                        (point-min))))
        (unless (nth 4 args)
                (setf (nth 4 args)
                (if (region-active-p)
                        (region-end)
                        (point-max))))
        (apply oldfun args))
(advice-add 'replace-string :around 'advice--replace-whole-buffer)
(advice-add 'query-replace :around 'advice--replace-whole-buffer)

(map! :n "C-s" 'replace-string
      :n "C-S-s" 'query-replace)

(add-hook 'spell-fu-mode-hook
  (lambda ()
    (spell-fu-dictionary-add (spell-fu-get-ispell-dictionary "de"))
    (spell-fu-dictionary-add (spell-fu-get-ispell-dictionary "en"))
    ))
(setq ispell-personal-dictionary "~/Dropbox/.aspell.en.pws")

(setq langtool-java-classpath "/usr/share/languagetool/*")

(add-hook 'snippet-mode-hook 'my-snippet-mode-hook)
(defun my-snippet-mode-hook ()
  "Custom behaviours for `snippet-mode'."
  (setq-local require-final-newline nil)
  (setq-local mode-require-final-newline nil))

(setq yas-triggers-in-field t)

(setq yas-snippet-revival nil)

(use-package warnings
    :config
    (cl-pushnew '(yasnippet backquote-change)
                warning-suppress-types
                :test 'equal))

(setq yas-key-syntaxes '(yas-try-key-from-whitespace "w_.()" "w_." "w_" "w"))

(defadvice! include-backslash (oldfun)
  :around #'yas--templates-for-key-at-point
  (setq my-syntax-table (copy-syntax-table (syntax-table)))
  (modify-syntax-entry ?\\ "w" my-syntax-table)
  (with-syntax-table my-syntax-table (funcall oldfun)))

(defun yas-new-snippet-clone (&optional no-template)
  "Clone of `yas-new-snippet' to avoid Doom Emacs remapping keys."
  (interactive "P")
  (yas-new-snippet no-template))
(defun yas-visit-snippet-file-clone (&optional no-template)
  "Clone of `yas-visit-snippet-file' to avoid Doom Emacs remapping keys."
  (interactive)
  (yas-visit-snippet-file))
(map! :leader
      (:prefix ("y" . "YASnippet")
       :desc "edit snippet"             "e" #'yas-visit-snippet-file-clone
       :desc "edit snippet (doom ver.)" "E" #'+snippets/edit
       :desc "insert snippet"           "i" #'yas-insert-snippet
       :desc "new snippet"              "n" #'yas-new-snippet-clone
       :desc "new snippet (doom ver.)"  "N" #'+snippets/new
       :desc "find private snippet"     "p" #'+snippets/find-private
       :desc "reload all snippets"      "r" #'yas-reload-all))

(setq yas-new-snippet-default (concat "# -*- mode: snippet -*-\n"
                                  "# name: $1\n"
                                  "# uuid: $2\n"
                                  "# key: $3\n"
                                  "# condition: ${4:t}\n"
                                  "# --\n"
                                  "$0"))

(defun yas-try-expanding-auto-snippets ()
  (when (and (boundp 'yas-minor-mode) yas-minor-mode)
    (let ((yas-buffer-local-condition ''(require-snippet-condition . auto))
          ;(yas-key-syntaxes '(yas-try-key-from-whitespace "w_.()" "w_." "w_" "w"))
          )
      (yas-expand))))
(add-hook 'post-self-insert-hook #'yas-try-expanding-auto-snippets)

(after! company
        (map! :map company-search-map
                [tab] nil
                "TAB" nil)
        (map! :map company-active-map
                [tab] nil
                "TAB" nil))

(defun cdlatex-in-yas-field ()
        ;; Check if we're at the end of the Yas field
        (when-let* ((_ (overlayp yas--active-field-overlay))
                        (end (overlay-end yas--active-field-overlay)))
        (if (>= (point) end)
                ;; Call yas-next-field if cdlatex can't expand here
                (let ((s (thing-at-point 'sexp)))
                (unless (and s (assoc (substring-no-properties s)
                                        cdlatex-command-alist-comb))
                (yas-next-field-or-maybe-expand)
                t))
                ;; otherwise expand and jump to the correct location
                (let (cdlatex-tab-hook minp)
                (setq minp
                        (min (save-excursion (cdlatex-tab)
                                        (point))
                        (overlay-end yas--active-field-overlay)))
                (goto-char minp) t))))

(defun yas-next-field-or-cdlatex nil
        (interactive)
        "Jump to the next Yas field correctly with cdlatex active."
        (if
                (or (bound-and-true-p cdlatex-mode)
                (bound-and-true-p org-cdlatex-mode))
                (cdlatex-tab)
        (yas-next-field-or-maybe-expand)))

(after! cdlatex
        (add-hook 'cdlatex-tab-hook 'yas-expand)
        (add-hook 'cdlatex-tab-hook 'cdlatex-in-yas-field))
(after! yasnippet
        (map! :map yas-keymap
                [tab] 'yas-next-field-or-cdlatex
                "TAB" 'yas-next-field-or-cdlatex))

(defun cdlatex-tab-or-indent ()
  "Indent when at the beginning of a line (or if current point is preceeded only by whitespace). Otherwise, call `cdlatex-tab'."
  (interactive)
  (if (or (bolp) (looking-back "^[ \t]+"))
      (indent-for-tab-command)
      (cdlatex-tab)))
(map! :map LaTeX-mode-map
                :i [tab] 'cdlatex-tab-or-indent
                :i "TAB" 'cdlatex-tab-or-indent)

(setq jit-lock-defer-time 0.25)

(setq org-element-use-cache nil)

(defun helpful-toggle-edebug-query ()
  "Query the user for a function, and then toggle edebug for that function using `helpful--toggle-edebug'."
  (interactive)
  (helpful--toggle-edebug (car (help-fns--describe-function-or-command-prompt))))

(map! :leader
      (:prefix ("d" . "debugging")
       :desc "edebug defun at point"            "d" #'edebug-defun
       :desc "debug on entry"                   "e" #'debug-on-entry
       :desc "cancel debug on entry"            "E" #'cancel-debug-on-entry
       :desc "toggle edebug for function"       "f" #'helpful-toggle-edebug-query
       :desc "remove edebug from function"      "F" #'edebug-remove-instrumentation
       :desc "view echo area messages"          "m" #'view-echo-area-messages
       :desc "start/stop profiler"              "p" #'doom/toggle-profiler
       :desc "trace function"                   "r" #'trace-function
       :desc "open sandbox"                     "s" #'doom/sandbox
       :desc "toggle debug on error"            "t" #'toggle-debug-on-error
       :desc "untrace function"                 "u" #'untrace-function
       :desc "untrace all functions"            "U" #'untrace-all
       :desc "debug on variable change"         "v" #'debug-on-variable-change
       :desc "cancel debug on variable change"  "V" #'cancel-debug-on-variable-change))

(setq ;org-directory "~/org/"
      org-roam-directory "~/Dropbox/roam"
      org-cd-directory (concat org-roam-directory "/tikz-cd")) ; for commutative diagrams
;;(setq org-agenda-files (list "~/org/todo.org" "~/org/lv_Sommer2023.org"))
(setq org-agenda-files nil) ;currently not using org-agenda
(setq org-directory nil) ;currently not using org-agenda

(after! org-modern
  (setq org-modern-star 'replace
        org-modern-replace-stars "❭"
        org-modern-list '((?+ . ?✦) (?- . ?➤))
        org-modern-todo nil
        org-modern-table nil
        org-modern-timestamp '(" %d. %b %Y " . " %H:%M ")))
(after! org
  (setq org-ellipsis " ▼ "
        org-hide-emphasis-markers t
        org-appear-autoemphasis t
        org-appear-autosubmarkers t
        org-appear-autolinks nil
        org-hidden-keywords '(title)
        org-image-align 'center
        org-todo-keyword-faces '(("WAIT" . "#ECBE7B")
                                ("TODELEGATE" . "pink")
                                ("IDEA" . "cyan")
                                ("DONE" . "#5b8c68")
                                ("DELEGATED" . "#a9a1e1")
                                ("CANCELLED" . "#ff6c6b"))))
(custom-set-faces!
  `(org-level-1 :inherit outline-1 :height 1.4)
  `(org-level-2 :inherit outline-2 :height 1.25)
  `(org-level-3 :inherit outline-3 :height 1.1)
  `(org-level-4 :inherit outline-4 :height 1.05)
  `(org-level-5 :inherit outline-5 :height 1.0)
  `(org-document-title :family "K2D" :foreground "#9BDB4D" :background nil :height 2.0))

(after! org
  (setq org-pretty-entities-include-sub-superscripts nil))

(after! org
  (setq
        org-log-done 'time
        org-agenda-skip-scheduled-if-done t
        org-agenda-skip-deadline-if-done t
        org-deadline-warning-days 7
        org-todo-keywords '((sequence
             "TODO(t)"
             "WAIT(w)"
             "TODELEGATE(T)"
             "IDEA(i)"
             "|"
             "DONE(d)"
             "DELEGATED(D)"
             "CANCELLED(c)" ))))

(setq org-roam-default-template '("d" "default" plain "%?" :target
            (file+head "%<%Y%m%d%H%M%S>-${slug}.org" "#+filetags: :draft:\n#+title: ${title}\n\n")
        :unnarrowed t :immediate-finish t))

(setq org-roam-capture-templates (list org-roam-default-template))

(defun org-roam-node-insert-default (&optional FILTER-FN &key INFO)
        "org-roam-node-insert, but it always uses the default template"
        (interactive)
        (org-roam-node-insert FILTER-FN :templates (list org-roam-default-template) :info INFO))
(defun org-roam-node-find-default (&optional OTHER-WINDOW INITIAL-INPUT FILTER-FN PRED)
        "org-roam-node-find, but it always uses the default template"
        (interactive current-prefix-arg)
        (org-roam-node-find OTHER-WINDOW INITIAL-INPUT FILTER-FN PRED :templates (list org-roam-default-template)))
(defun org-roam-capture-default (&optional GOTO KEYS &key FILTER-FN INFO)
        "org-roam-capture, but it always uses the default template"
        (interactive "P")
        (org-roam-capture GOTO KEYS :filter-fn FILTER-FN :templates (list org-roam-default-template) :info INFO))

(defadvice org-roam-node-insert (around append-if-in-evil-normal-mode activate compile)
  "If in evil normal mode and cursor is on a whitespace character, then go into
append mode first before inserting the link. This is to put the link after the
space rather than before."
  (let ((is-in-evil-normal-mode (and (bound-and-true-p evil-mode)
                                     (not (bound-and-true-p evil-insert-state-minor-mode))
                                     (looking-at "[[:blank:]]"))))
    (if (not is-in-evil-normal-mode)
        ad-do-it
      (evil-append 0)
      ad-do-it
      (evil-normal-state))))

(use-package! websocket
    :after org-roam)

(use-package! org-roam-ui
    :after org-roam ;; or :after org
;;         normally we'd recommend hooking orui after org-roam, but since org-roam does not have
;;         a hookable mode anymore, you're advised to pick something yourself
;;         if you don't care about startup time, use
;;  :hook (after-init . org-roam-ui-mode)
    :config
    (setq org-roam-ui-sync-theme t
          org-roam-ui-follow t
          org-roam-ui-update-on-save t
          org-roam-ui-open-on-start nil))

;;; org-roam-link-properties.el --- Frobnicate and bifurcate flanges

;; Author: Oleg Domanov <odomanov@yandex.ru>
;; Version: 1.0
;; Keywords: org-roam org-roam-ui

;;; Commentary:

;;;  Org-Roam link properties (for 'id' links only).
;;;  Adapted from https://linevi.ch/en/org-link-extra-attrs.html

;;; Code:

(defun odm/org-link-extra-attrs (orig-fun &rest args)
  "Post processor for parsing links"
  (setq parser-result orig-fun)

  ;;; Retrieving inital values that should be replaced
  (setq raw-path (plist-get (nth 1 parser-result) :raw-link))

  ;; check if raw-path is not nil
  (if raw-path
        ;; Checking if link match the regular expression
        (if (string-match-p "^id:.*|\s*:" raw-path)
        (progn
                ;; Retrieving parameters after the vertical bar
                (setq results (s-split "|" raw-path))
                (setq raw-path (car results))
                (setq path (s-chop-prefix "id:" raw-path))

                ;; Cleaning, splitting and making symbols
                (setq results (s-split "\s" (s-trim (s-collapse-whitespace
                                                (car (-slice results 1))))))
                (setq results (--map (intern it) results))

                ;; Updating the ouput with the new values
                (setq orig-fun-cleaned (plist-put (nth 1 orig-fun) :raw-link raw-path))
                (setq orig-fun-cleaned (plist-put orig-fun-cleaned :path path))

                ;; Check that the number is even
                (if (= 2 (length (-last-item (-partition-all 2 results))))
                (list 'link (-snoc orig-fun-cleaned :extra-attrs results))
                (progn
                (message "Links properties are incorrect.")
                (list 'link orig-fun-cleaned))))

    ;; Or returning original value of the function
    orig-fun)))

(advice-add 'org-element-link-parser :filter-return #'odm/org-link-extra-attrs)

(defun odm/org-roam-db-extra-properties (link)
  "Append extra-attrs to the LINK's properties."
  (save-excursion
    (goto-char (org-element-property :begin link))
    (let ((path (org-element-property :path link))
          (source (org-roam-id-at-point))
          (extra-attrs (org-element-property :extra-attrs link)))
      (when extra-attrs
        (setq properties (caar (org-roam-db-query
                               [:select properties :from links
                                        :where (= source $s1) :and (= dest $s2)
                                        :limit 1]
                               source path)))
        (setq properties (append properties extra-attrs))
        (when (and source path)
          (org-roam-db-query
           [:update links :set (= properties $s3)
                    :where (= source $s1) :and (= dest $s2)]
           source path properties))))))

(advice-add 'org-roam-db-insert-link :after #'odm/org-roam-db-extra-properties)

(provide 'org-roam-link-properties)

;;; org-roam-link-properties.el ends here

(defun org-link-set-tags (&optional tags link)
  "Set the tags of the link at point."
  (interactive)
  (save-excursion
    (save-match-data
      (let* ((tags (or tags (read-string "Tags: ")))
             (link (or link (org-element-context)))
             (raw-link (org-element-property :raw-link link))
             (path (org-element-property :path link))
             (desc (and (org-element-property :contents-begin link)
                        (org-element-property :contents-end link)
                        (buffer-substring-no-properties
                         (org-element-property :contents-begin link)
                         (org-element-property :contents-end link))))
             node)
        (goto-char (org-element-property :begin link))
        (when (org-in-regexp org-link-any-re 1)
          (replace-match (org-link-make-string
                          (concat raw-link "|:tag " tags)
                          (or desc path))))))))

(defun org-link-remove-tags (&optional link)
  "Remove the tags of the link at point."
  (interactive)
  (save-excursion
    (save-match-data
      (let* ((link (or link (org-element-context)))
             (raw-link (org-element-property :raw-link link))
             (path (org-element-property :path link))
             (desc (and (org-element-property :contents-begin link)
                        (org-element-property :contents-end link)
                        (buffer-substring-no-properties
                         (org-element-property :contents-begin link)
                         (org-element-property :contents-end link))))
             node)
        (goto-char (org-element-property :begin link))
        (when (org-in-regexp org-link-any-re 1)
          (replace-match (org-link-make-string
                          raw-link
                          (or desc path))))))))

(defun org-roam-implication-tag ()
  "Tag link at point as implication"
  (interactive)
  (org-link-set-tags "implication")
  )
(defun org-roam-implication-insert (&optional FILTER-FN &key INFO)
  "org-roam-node-insert-default, but the link is tagged with \"implication\""
  (interactive)
  (org-roam-node-insert-default FILTER-FN :key INFO)
  (org-link-set-tags "implication")
  )

(defun commutative-diagram-filename-generate ()
  (setq commutative-diagram-filename--name (read-string "Name: "))
  (setq commutative-diagram-filename--time (format-time-string "%Y%m%d%H%M%S"))
  (setq commutative-diagram-filename--image (expand-file-name (format "%s-%s.png" commutative-diagram-filename--time commutative-diagram-filename--name) org-cd-directory))
  (setq commutative-diagram-filename--org (expand-file-name (format "%s-%s.org" commutative-diagram-filename--time commutative-diagram-filename--name) org-cd-directory)))

(after! org-capture (add-to-list 'org-capture-templates
  '("c" "Commutative Diagram" plain
     (file commutative-diagram-filename-generate)
     "%(format \"#+TITLE: %s\n#+STAMP: %s\n#+HEADER: :imagemagick yes :iminoptions -density 600 -geometry 1500 :buffer no :fit yes \n#+HEADER: :results raw  :file %s-%s.png \n#+HEADER: :packages '((\\\"\\\" \\\"tikz-cd\\\")) \n#+HEADER: :exports results :results output graphics file \n#+BEGIN_SRC latex \n\\\\begin{tikzcd}[white]\n %%? \n\\\\end{tikzcd}\n#+END_SRC\" commutative-diagram-filename--name commutative-diagram-filename--time commutative-diagram-filename--time commutative-diagram-filename--name)")))

(defun org-capture-commutative-diagram--render ()
    (when (and (not org-note-abort) (equal (plist-get org-capture-plist :key) "c")) ; execute only for the commutative diagram capture template
    (org-babel-execute-buffer)))
(after! org-capture (add-hook 'org-capture-before-finalize-hook 'org-capture-commutative-diagram--render))

(defun org-capture-commutative-diagram--insert-link () (interactive)
  (when (and (not org-note-abort) (equal (plist-get org-capture-plist :key) "c")) ; execute only for the commutative diagram capture template
    (evil-open-below 1)
    (insert "[[" commutative-diagram-filename--image "]]\n")
    (evil-normal-state)
    (org-redisplay-inline-images)
))
(after! org-capture (add-hook 'org-capture-after-finalize-hook 'org-capture-commutative-diagram--insert-link))

(defun org-capture-commutative-diagram () (interactive)
    (org-capture nil "c")
)

(map! :leader
      (:prefix ("r" . "roam")
         :desc "Open random node"                       "0" #'org-roam-node-random
         :desc "Find node (default template)"           "f" #'org-roam-node-find-default
         :desc "Find node (choose template)"            "F" #'org-roam-node-find
         :desc "Show UI"                                "g" #'org-roam-ui-open
         :desc "Insert node (default template)"         "i" #'org-roam-node-insert-default
         :desc "Insert node (choose template)"          "I" #'org-roam-node-insert
         :desc "Insert implication"                     "j" #'org-roam-implication-insert
         :desc "Tag link as implication"                "J" #'org-roam-implication-tag
         :desc "Capture to node (default template)"     "n" #'org-roam-capture-default
         :desc "Capture to node (choose template)"      "N" #'org-roam-capture
         :desc "Toggle roam buffer"                     "r" #'org-roam-buffer-toggle
         :desc "Launch roam buffer"                     "R" #'org-roam-buffer-display-dedicated
         :desc "Sync database"                          "s" #'org-roam-db-sync
         :desc "Add tag"                                "t" #'org-roam-tag-add
         :desc "Remove tag"                             "T" #'org-roam-tag-remove
         :desc "Set link tags"                          "l" #'org-link-set-tags
         :desc "Remove link tags"                       "L" #'org-link-remove-tags
         :desc "Add alias"                              "a" #'org-roam-alias-add
         :desc "Remove alias"                           "A" #'org-roam-alias-remove
         :desc "Commutative diagram"                    "c" #'org-capture-commutative-diagram
         (:prefix ("d" . "by date")
          :desc "Goto previous note"                    "b" #'org-roam-dailies-goto-previous-note
          :desc "Goto date"                             "d" #'org-roam-dailies-goto-date
          :desc "Capture date"                          "D" #'org-roam-dailies-capture-date
          :desc "Goto next note"                        "f" #'org-roam-dailies-goto-next-note
          :desc "Goto tomorrow"                         "m" #'org-roam-dailies-goto-tomorrow
          :desc "Capture tomorrow"                      "M" #'org-roam-dailies-capture-tomorrow
          :desc "Capture today"                         "n" #'org-roam-dailies-capture-today
          :desc "Goto today"                            "t" #'org-roam-dailies-goto-today
          :desc "Capture today"                         "T" #'org-roam-dailies-capture-today
          :desc "Goto yesterday"                        "y" #'org-roam-dailies-goto-yesterday
          :desc "Capture yesterday"                     "Y" #'org-roam-dailies-capture-yesterday
          :desc "Find directory"                        "-" #'org-roam-dailies-find-directory)))

(map! :after org
    :map org-mode-map
    :localleader
    :prefix ("u" . "org-roam-ui")
    "o" #'org-roam-ui-open
    "z" #'org-roam-ui-node-zoom
    "l" #'org-roam-ui-node-local
    "T" #'org-roam-ui-sync-theme
    "f" #'org-roam-ui-follow-mode
    "a" #'org-roam-ui-add-to-local-graph
    "c" #'org-roam-ui-change-local-graph
    "r" #'org-roam-ui-remove-from-local-graph)

(defvar roam-pseudohook nil
 "A hook run only on org files in org-roam-directory.")
(defun roam-pseudohook-function ()
  (cond ((string-prefix-p org-roam-directory (buffer-file-name))
         (run-hooks 'roam-pseudohook)
         )))
(after! org (add-hook 'org-mode-hook 'roam-pseudohook-function))

(add-hook 'roam-pseudohook (lambda () (setq-local +word-wrap-fill-style 'soft) (+word-wrap-mode 1)))
(add-hook 'roam-pseudohook (lambda () (mixed-pitch-mode 1)))

(defun writeroom-mode-deactivate () (writeroom-mode -1))
(add-hook 'org-roam-capture-new-node-hook 'writeroom-mode-deactivate)
(add-hook 'org-capture-mode-hook 'writeroom-mode-deactivate)

(setq org-roam-node-display-template
      (concat "${title:*} "
              (propertize "${tags:30}" 'face 'org-tag))) ; 30 is the max. number of characters allocated for tags

(defun org-latex-preview-check-health (&optional inter)
  "Inspect the relevent system state and setup.
INTER signals whether the function has been called interactively."
  (interactive (list t))
  ;; Collect information
  (let* ((diag `(:interactive ,inter)))
    (plist-put diag :org-version org-version)
    ;; modified variables
    (plist-put diag :modified
               (let ((list))
                 (mapatoms
                  (lambda (v)
                    (and (boundp v)
                         (string-match "\\`\\(org-latex-\\|org-persist-\\)" (symbol-name v))
                         (or (and (symbol-value v)
                                  (string-match "\\(-hook\\|-function\\)\\'" (symbol-name v)))
                             (and
                              (get v 'custom-type) (get v 'standard-value)
                              (not (equal (symbol-value v)
                                          (eval (car (get v 'standard-value)) t)))))
                         (push (cons v (symbol-value v)) list))))
                 list))
    ;; Executables
    ;; latex processors
    (dolist (processor org-latex-compilers)
      (when-let ((path (executable-find processor)))
        (let ((version (with-temp-buffer
                         (thread-last
                           (concat processor " --version")
                           (shell-command-to-string)
                           (insert))
                         (goto-char (point-min))
                         (buffer-substring (point) (line-end-position)))))
          (push (list processor version path) (plist-get diag :latex-processors)))))
    ;; Image converters
    (dolist (converter '("dvipng" "dvisvgm" "convert"))
      (when-let ((path (executable-find converter)))
        (let ((version (with-temp-buffer
                         (thread-last
                           (concat converter " --version")
                           (shell-command-to-string)
                           (insert))
                         (goto-char (point-min))
                         (buffer-substring (point) (line-end-position)))))
          (push (list converter version path) (plist-get diag :image-converters)))))
    (when inter
      (with-current-buffer (get-buffer-create "*Org LaTeX Preview Report*")
        (let ((inhibit-read-only t))
          (erase-buffer)

          (insert (propertize "Your LaTeX preview process" 'face 'outline-1))
          (insert "\n\n")

          (let* ((latex-available (cl-member org-latex-compiler
                                             (plist-get diag :latex-processors)
                                             :key #'car :test #'string=))
                 (precompile-available
                  (and latex-available
                       (not (member org-latex-compiler '("lualatex" "xelatex")))))
                 (proc-info (alist-get
                             org-latex-preview-process-default
                             org-latex-preview-process-alist))
                 (image-converter (cadr (plist-get proc-info :programs)))
                 (image-converter
                  (cl-find-if
                   (lambda (c)
                     (string= image-converter c))
                   (plist-get diag :image-converters)
                   :key #'car))
                 (image-output-type (plist-get proc-info :image-output-type)))
            (if org-latex-preview-process-precompiled
                (insert "Precompile with "
                        (propertize (map-elt org-latex-precompile-compiler-map
                                             org-latex-compiler)
                                    'face
                                    (list
                                     (if precompile-available
                                         '(:inherit success :box t)
                                       '(:inherit error :box t))
                                     'org-block))
                        " → "))
            (insert "LaTeX Compile with "
                    (propertize org-latex-compiler 'face
                                (list
                                 (if latex-available
                                     '(:inherit success :box t)
                                   '(:inherit error :box t))
                                 'org-block))
                    " → ")
            (insert "Convert to "
                    (propertize (upcase image-output-type) 'face '(:weight bold))
                    " with "
                    (propertize (car image-converter) 'face
                                (list
                                 (if image-converter
                                     '(:inherit success :box t)
                                   '(:inherit error :box t))
                                 'org-block))
                    "\n\n")
            (insert (propertize org-latex-compiler 'face 'outline-3)
                    "\n"
                    (if latex-available
                        (concat
                          (propertize
                           (mapconcat #'identity (map-nested-elt diag `(:latex-processors ,org-latex-compiler))
                                      "\n")
                           'face 'org-block)
                          "\n"
                          (when (and latex-available (not precompile-available))
                            (propertize
                             (format "\nWarning: Precompilation not available with %S!\n" org-latex-compiler)
                             'face 'warning)))
                      (propertize "Not found in path!\n" 'face 'error))
                    "\n")

            (insert (propertize (cadr (plist-get proc-info :programs)) 'face 'outline-3)
                    "\n"
                    (if image-converter
                        (propertize
                         (concat
                          (mapconcat #'identity (cdr image-converter) "\n")
                          "\n")
                         'face 'org-block)
                      (propertize "Not found in path!\n" 'face 'error))
                    "\n")
            ;; dvisvgm version check
            (when (equal (car-safe image-converter)
                         "dvisvgm")
              (let* ((version-string (cadr image-converter))
                     (dvisvgm-ver (progn
                                    (string-match "\\([0-9.]+\\)" version-string)
                                    (match-string 1 version-string))))

                (when (version< dvisvgm-ver "3.0")
                  (insert (propertize
                           (format "Warning: dvisvgm version %s < 3.0, displaymath will not be centered."
                                   dvisvgm-ver)
                           'face 'warning)
                          "\n\n"))))
            (when (not (and latex-available image-converter))
              (insert "path: " (getenv "PATH") "\n\n")))
          ;; Settings
          (insert (propertize "LaTeX preview options" 'face 'outline-2)
                  "\n")

          (pcase-dolist (`(,var . ,msg)
                         `((,org-latex-preview-process-precompiled . "Precompilation           ")
                           (,org-latex-preview-numbered . "Equation renumbering     ")
                           (,org-latex-preview-cache  . "Caching with org-persist ")))
            (insert (propertize "• " 'face 'org-list-dt)
                    msg
                    (if var
                        (propertize "ON" 'face '(success bold org-block))
                      (propertize "OFF" 'face '(error bold org-block)))
                    "\n"))
          (insert "\n"
                  (propertize "LaTeX preview sizing" 'face 'outline-2) "\n"
                  (propertize "•" 'face 'org-list-dt)
                  " Page width  "
                  (propertize
                   (format "%S" (plist-get org-latex-preview-appearance-options :page-width))
                   'face '(org-code org-block))
                  "   (display equation width in LaTeX)\n"
                  (propertize "•" 'face 'org-list-dt)
                  " Scale       "
                  (propertize
                   (format "%.2f" (plist-get org-latex-preview-appearance-options :scale))
                   'face '(org-code org-block))
                  "  (PNG pixel density multiplier)\n"
                  (propertize "•" 'face 'org-list-dt)
                  " Zoom        "
                  (propertize
                   (format "%.2f" (plist-get org-latex-preview-appearance-options :zoom))
                   'face '(org-code org-block))
                  "  (display scaling factor)\n\n")
          (insert (propertize "LaTeX preview preamble" 'face 'outline-2) "\n")
          (let ((major-mode 'org-mode))
            (let ((point-1 (point)))
              (insert org-latex-preview-preamble "\n")
              (org-src-font-lock-fontify-block 'latex point-1 (point))
              (add-face-text-property point-1 (point) '(:inherit org-block :height 0.9)))
            (insert "\n")
            ;; Diagnostic output
            (insert (propertize "Diagnostic info (copied)" 'face 'outline-2)
                    "\n\n")
            (let ((point-1 (point)))
              (pp diag (current-buffer))
              (org-src-font-lock-fontify-block 'emacs-lisp point-1 (point))
              (add-face-text-property point-1 (point) '(:height 0.9))))
          (gui-select-text (prin1-to-string diag))
          (special-mode))
        (setq-local
         revert-buffer-function
         (lambda (&rest _)
           (call-interactively #'org-latex-preview-check-health)
           (message "Refreshed LaTeX preview diagnostic")))
        (let ((message-log-max nil))
          (toggle-truncate-lines 1))
        (goto-char (point-min))
        (display-buffer (current-buffer))))
    diag))

(use-package! org-latex-preview
  :config
  ;; Increase preview width & zoom
  (plist-put org-latex-preview-appearance-options
             :page-width 0.8)
  (plist-put org-latex-preview-appearance-options
             :zoom 1.2)

  (setq org-latex-packages-alist '(
        ("" "amsmath" t ("pdflatex"))
        ("" "amssymb" t ("pdflatex"))
        ("" "tikz" t ("pdflatex" "lualatex" "xetex"))
        ("" "pgfplots" t ("pdflatex" "lualatex" "xetex"))))
  (setq org-latex-preview-preamble (concat org-latex-preview-preamble "\n\\pgfplotsset{compat=1.16}\\usetikzlibrary{cd}\n"))

  (setq org-latex-compiler "pdflatex")

  ;; Use dvisvgm to generate previews
  ;; You don't need this, it's the default:
  (setq org-latex-preview-process-default 'dvisvgm)

  ;; Turn on auto-mode, it's built into Org and much faster/more featured than
  ;; org-fragtog. (Remember to turn off/uninstall org-fragtog.)
  (add-hook 'org-mode-hook 'org-latex-preview-auto-mode)

  ;; Block C-n and C-p from opening up previews when using auto-mode
  (add-hook 'org-latex-preview-auto-ignored-commands 'next-line)
  (add-hook 'org-latex-preview-auto-ignored-commands 'previous-line)

  ;; Bonus: Turn on live previews.  This shows you a live preview of a LaTeX
  ;; fragment and updates the preview in real-time as you edit it.
  ;; To preview only environments, set it to '(block edit-special) instead
  (setq org-latex-preview-live t)

  ;; More immediate live-previews -- the default delay is 1 second
  (setq org-latex-preview-live-debounce 0.25))

(defun org-latex-preview-clear ()
  "Disable org-latex-preview (which is the same as running org-latex-preview with prefix argument)"
  (interactive)
  (let ((current-prefix-arg '(4)))
    (call-interactively 'org-latex-preview)))
(defun org-latex-preview-whole-buffer ()
  "Render all previews in buffer (which is the same as running org-latex-preview with a double prefix argument)"
  (interactive)
  (let ((current-prefix-arg '(16)))
    (call-interactively 'org-latex-preview)))

(add-hook 'roam-pseudohook 'org-latex-preview-whole-buffer)

(setq org-latex-mathoperators (list
        "acl" "Ad" "Aut" "bd" "Binom" "card" "Cb" "CB" "cl" "coker" "Cov" "Covar" "dcl" "ded" "dist" "ED" "EM" "End" "Exp" "Ext" "fr" "Frac" "Gal" "Geom" "GL" "Hom" "id" "im" "ind" "lexmin" "lexmax" "Li" "Mat" "ord" "Poisson" "Pois" "RM" "sinc" "SL" "SO" "Spec" "st" "Stab" "SU" "Sub" "Th" "tp" "Tor" "Unif" "Var"))
(dolist (macro org-latex-mathoperators)
  (setq org-latex-preview-preamble (concat org-latex-preview-preamble "\\DeclareMathOperator{\\" macro "}{" macro "}"))
  (add-to-list 'org-roam-ui-latex-macros (cons (concat "\\" macro) (concat "\\operatorname{" macro "}")) t)
  )

(setq org-latex-preview-preamble (concat org-latex-preview-preamble
                                         "\\DeclareMathOperator{\\ch}{char}"))
(add-to-list 'org-roam-ui-latex-macros (cons "\\ch" "\\operatorname{char}") t)

(setq org-latex-preview-preamble (concat org-latex-preview-preamble
"\\newcommand{\\fork}[1][]{%
  \\mathrel{
    \\mathop{
      \\vcenter{
        \\hbox{\\oalign{\\noalign{\\kern-.3ex}\\hfil$\\vert$\\hfil\\cr
              \\noalign{\\kern-.7ex}
              $\\smile$\\cr\\noalign{\\kern-.3ex}}}
      }
    }\\displaylimits_{#1}
  }
}"))

(require 'org-src)
(add-to-list 'org-src-block-faces '("latex" (:inherit default :extend t)))

(require 'smartparens-config)
  (sp-local-pair 'org-mode "\\[" "\\]")
  (sp-local-pair 'org-mode "$" "$")
  (sp-local-pair 'org-mode "'" "'" :actions '(rem))
  (sp-local-pair 'org-mode "=" "=" :actions '(rem))
  (sp-local-pair 'org-mode "\\left(" "\\right)" :trigger "\\l(" :post-handlers '(sp-latex-insert-spaces-inside-pair))
  (sp-local-pair 'org-mode "\\left[" "\\right]" :trigger "\\l[" :post-handlers '(sp-latex-insert-spaces-inside-pair))
  (sp-local-pair 'org-mode "\\left\\{" "\\right\\}" :trigger "\\l{" :post-handlers '(sp-latex-insert-spaces-inside-pair))
  (sp-local-pair 'org-mode "\\left|" "\\right|" :trigger "\\l|" :post-handlers '(sp-latex-insert-spaces-inside-pair))

;; Org Notebook
(setq org-notebook-result-dir "./handwritten/")
(setq org-notebook-template-path "~/Dropbox/template.xopp")

(defun org-notebook-get-png-link-at-point (shouldThrowError)
    "Returns filepath of org link at cursor"
    (setq linestr (thing-at-point 'line))
    (setq start (string-match "\\[\\[" linestr))
    (setq end (string-match "\\]\\]" linestr))
    (if shouldThrowError (if start nil (error "No link found")) nil)
    (if shouldThrowError (if end nil   (error "No link found")) nil)
    (if shouldThrowError (if (string-match ".png" linestr) nil   (error "Link is not an image")) nil)

    (if (and linestr start end) (substring linestr (+ start 2) end) nil)
)

(defun org-notebook-gen-filename-at-point ()
    "Returns a list of valid file paths corresponding to current context(Header & Date)."

    (unless (file-directory-p org-notebook-result-dir) (make-directory org-notebook-result-dir))

    (setq date-string (format-time-string "%Y-%m-%d_%H%M%S"))

    ; return current heading if available
    ; otherwise return title of org document
    ; if that's also not available, return nil
    (setq heading (condition-case nil
            (nth 4 (org-heading-components))
            (error (if (org-collect-keywords '("TITLE"))
                (nth 1 (nth 0 (org-collect-keywords '("TITLE"))))
                ""
            ))))


    (setq heading (replace-regexp-in-string "\\[.*\\]" "" heading))

    ;; First filter out weird symbols
    (setq heading (replace-regexp-in-string "[/;:'\"\(\)]+" "" heading))
    (setq heading (string-trim heading))
    ;; filter out swedish characters åäö -> aao
    (setq heading(replace-regexp-in-string "[åÅäÄ]+" "a" heading))
    (setq heading(replace-regexp-in-string "[öÓ]+" "o" heading))
    ;; whitespace and . to underscores
    (setq heading (replace-regexp-in-string "[ .]+" "_" heading))

    (setq filename (format "%s-%s" heading date-string))
    (setq filename (read-minibuffer "Filename: " filename))

    (setq image-path (format "%s%s.png" org-notebook-result-dir filename))
    (setq xournal-path (format "%s%s.xopp" org-notebook-result-dir filename))

    (list image-path xournal-path)
)


(defun org-notebook-create-xournal ()
    "Insert an image and open the drawing program"
    (interactive)

    (setq notebookfile (org-notebook-gen-filename-at-point))
    (setq image-path (car notebookfile))
    (setq xournal-path (nth 1 notebookfile))

    (evil-open-below 1)
    (insert "[[" image-path "]]\n")
    (evil-normal-state)

    (start-process-shell-command "org-notebook-copy-template" nil (concat "cp " org-notebook-template-path " " xournal-path))
    (start-process "org-notebook-drawing" nil "xournalpp" xournal-path)
)

(defun org-notebook-edit-xournal ()
    (interactive)
    (setq image-path (org-notebook-get-png-link-at-point nil))
    (if (not image-path)
        (if (y-or-n-p "No matching xournal file, create one?")
            (org-notebook-create-xournal)
            (error "Nothing more to do...")
            )
            nil
        )

    (setq xournal-path (replace-regexp-in-string "\.png" ".xopp" image-path))
    (if (file-readable-p xournal-path) (start-process "org-notebook-drawing" nil "xournalpp" xournal-path) (error "No matching xournal file found"))
)

(defun org-notebook-generate-xournal-image ()
    (interactive)
    (setq image-path (org-notebook-get-png-link-at-point t))
    (setq xournal-path (replace-regexp-in-string "\.png" ".xopp" image-path))
    (if (file-readable-p xournal-path) nil (error "No matching xournal file found"))

    (setq xournal_cmd (format "xournalpp --export-no-background %s %s %s" xournal-path "-i" image-path))
    (print (format "Generating image file: %s" xournal_cmd))
    (shell-command xournal_cmd)


    (setq convert_cmd (format "convert %s -trim -bordercolor none -border 20 +repage %s" image-path image-path))
    (print (format "Auto cropping image: %s" convert_cmd))
    (shell-command convert_cmd)

    (org-redisplay-inline-images)
)


(map! :after org
    :map org-mode-map
    :localleader
    :prefix ("x" . "Xournal")
    "x" #'org-notebook-create-xournal
    "g" #'org-notebook-generate-xournal-image
    "e" #'org-notebook-edit-xournal)

(map! :localleader
      :map org-mode-map
      (:prefix ("D" . "org-d20")
       :desc "start/advance combat" "i" #'org-d20-initiative-dwim
       :desc "add to combat" "a" #'org-d20-initiative-add
       :desc "apply damage at point" "d" #'org-d20-damage
       :desc "roll" "r" #'org-d20-roll
       )
      )

(set-file-template! #'LaTeX-mode :mode #'latex-mode)

(setq evil-tex-toggle-override-m nil) ;; I want to use m for "move" (evil-cut)
;;... so I map toggle keybindings somewhere else instead
(map! :ni "C-t" nil) ;; unmap +workspace/new
(map! :map (evil-tex-mode-map org-mode-map)
      (:prefix ("C-t" . "toggle")
       :desc "command"          "c"     #'evil-tex-toggle-command
       :desc "delimiter"        "d"     #'evil-tex-toggle-delim
       :desc "environment"      "e"     #'evil-tex-toggle-env
       :desc "math"             "m"     #'evil-tex-toggle-math
       :desc "math align*"      "M"     #'evil-tex-toggle-math-align
       :desc "section"          "S"     #'evil-tex-toggle-section))

(setq +latex-indent-item-continuation-offset 'auto)

(add-hook 'LaTeX-mode-hook (lambda () (setq TeX-command-default "LaTeXMk")))

(setq flycheck-global-modes '(not LaTeX-mode latex-mode))

(add-hook 'TeX-mode-hook 'rainbow-delimiters-mode-disable
          'LaTeX-mode-hook 'rainbow-delimiters-mode-disable)
(after! latex
  (remove-hook 'TeX-update-style-hook #'rainbow-delimiters-mode))

(map! :localleader
      :map evil-tex-mode-map
      :desc "TeX-next-error"
      "e" #'TeX-next-error)

(setq TeX-quote-after-quote t) ; how this is supposed to work, for good measure

(defun insert-standard-quote ()
        "insert a completely normal quotation mark, bypassing weird AUCTex-defaults"
        (interactive)
        (insert "\""))
(map! :after tex
      :map tex-mode-map
      "\"" 'insert-standard-quote)
(map! :after tex
      :map LaTeX-mode-map
      "\"" 'insert-standard-quote)

(setq major-mode-remap-alist major-mode-remap-defaults)

(setq +latex-viewers '(zathura pdf-tools okular)
      TeX-view-program-selection '((output-pdf "Zathura") (output-pdf "Okular") (output-pdf "PDF Tools"))
      TeX-view-program-list '(("Zathura"
        ("zathura "
          (mode-io-correlate " --synctex-forward %n:0:%b -x \"emacsclient +%{line} %{input}\" ") " %o") "zathura")
                              ("PDF Tools" TeX-pdf-tools-sync-view)
                              ("Okular" ("okular --noraise --unique file:%o" (mode-io-correlate "#src:%n%a")))
                              ("preview-pane" latex-preview-pane-mode))
      TeX-source-correlate-start-server t)

;; Update PDF buffers after successful LaTeX runs
(add-hook 'TeX-after-compilation-finished-functions
           #'TeX-revert-document-buffer)

(add-hook 'TeX-mode-hook (lambda () (setq-local +word-wrap-fill-style 'soft) (+word-wrap-mode 1)))

(map! :after latex :map cdlatex-mode-map
      ; I'm too used to using the ' key to type stuff like "f prime"
      "\'"      nil
      ; so this key is better imo
      "\´"       #'cdlatex-math-modify
      "\`"       #'cdlatex-math-symbol
      )
(map! :map org-cdlatex-mode-map     ; same thing for within org mode
      "\'"      nil
      "\´"       #'cdlatex-math-modify
      "\`"       #'cdlatex-math-symbol
      )

(require 'cdlatex)
(setq cdlatex-math-modify-prefix 180)
(setq cdlatex-math-symbol-prefix 96)

(after! cdlatex
  (setq cdlatex-math-symbol-alist
   '( ;; adding missing functions to 3rd level symbols
     (?_    ("\\downarrow" "" "\\inf"))
     (?2    ("^2" "\\sqrt{?}" ""))
     (?3    ("^3" "\\sqrt[3]{?}" ""))
     (?^    ("\\uparrow" "" "\\sup"))
     (?k    ("\\kappa" "" "\\ker"))
     (?m    ("\\mu" "" "\\lim"))
     (?c    (""   "\\circ" "\\cos"))
     (?d    ("\\delta" "\\partial" ""))
     (?D    ("\\Delta" "\\nabla" "\\deg"))
     ;; no idea why \Phi isnt on 'F' in first place, \phi is on 'f'.
     (?F    ("\\Phi"))
     ;; varphi and phi are surely the wrong way around
     ;; similarly for epsilon
     (?f    ("\\varphi" "\\phi" ""))
     (?e    ("\\varepsilon" "\\exp" "\\epsilon"))
     (?s    ("\\sigma" "\\Sigma" "\\varsigma"))
     ;; now just convenience
     (?.    ("\\cdot" "\\dots"))
     (?:    ("\\vdots" "\\ddots"))
     (?*    ("\\times" "\\star" "\\ast")))
   cdlatex-math-modify-alist
   '((?B    "\\mathbb"        nil          t    nil  nil)
     (?o    "\\operatorname"  nil          t    nil  nil)
     (?a    "\\abs"           nil          t    nil  nil)
     (?f    "\\mathfrak"      nil          t    nil  nil)
     (?s    "\\mathsf"        nil          t    nil  nil))))

(setq reftex-label-alist
   '(("axiom"       ?a "ax:"  "~\\ref{%s}" 1 ("axiom"       "ax.")   -3)
     ("definition"  ?d "def:" "~\\ref{%s}" 1 ("definition"  "def.")  -3)
     ("corollary"   ?h "thm:" "~\\ref{%s}" 1 ("corollary"   "cor.")  -3)
     ("fact"        ?h "thm:" "~\\ref{%s}" 1 ("fact")                -3)
     ("lemma"       ?h "thm:" "~\\ref{%s}" 1 ("lemma"       "lem.")  -3)
     ("proposition" ?h "thm:" "~\\ref{%s}" 1 ("proposition" "prop.") -3)
     ("theorem"     ?h "thm:" "~\\ref{%s}" 1 ("theorem"     "thm.")  -3)))

(setq reftex-insert-label-flags '("sadh" "sftadh"))

(map! :localleader :map evil-tex-mode-map
      "l"       #'reftex-label
      "r"       (lambda () (interactive) (reftex-reference " ")) ; type "any"
      "R"       #'reftex-reference)

(setq! citar-bibliography '("/home/reiti/Zotero/biblioteca.bib"))
(setq! org-cite-global-bibliography citar-bibliography)

(setq citar-org-roam-subdir "/home/reiti/Dropbox/roam/literature")

(setq citar-org-roam-note-title-template "${author} - ${title}")

(add-to-list 'org-roam-capture-templates
  '("l" "Literature Note" plain
        "%?"
        :target
        (file+head
         "%(expand-file-name (or citar-org-roam-subdir \"\") org-roam-directory)/${citar-citekey}.org"
         ":PROPERTIES:\n:NOTER_DOCUMENT: %(if (string= \"\" \"%(citar-get-value \"file\" \"${citar-citekey}\")\") ( ) (print \"%(citar-get-value \"file\" \"${citar-citekey}\")\"))\n:NOTER_PAGE: 1\n:END:\n#+title: ${note-title}\n%(if (string= \"\" \"%(citar-get-value \"file\" \"${citar-citekey}\")\") (print \"${citar-citekey}\") (print \"[[file:%(citar-get-value \"file\" \"${citar-citekey}\")][${citar-citekey}]]\")), ${citar-date}\n\n")
        :unnarrowed t
     ) t)

(setq citar-org-roam-capture-template-key "l")

(after! citar
    (defvar citar-indicator-files-icons
      (citar-indicator-create
       :symbol (nerd-icons-faicon
                "nf-fa-file_o"
                :face 'nerd-icons-green
                :v-adjust -0.1)
       :function #'citar-has-files
       :padding "  " ; need this because the default padding is too low for these icons
       :tag "has:files"))
    (defvar citar-indicator-links-icons
      (citar-indicator-create
       :symbol (nerd-icons-faicon
                "nf-fa-link"
                :face 'nerd-icons-orange
                :v-adjust 0.01)
       :function #'citar-has-links
       :padding "  "
       :tag "has:links"))
    (defvar citar-indicator-notes-icons
      (citar-indicator-create
       :symbol (nerd-icons-codicon
                "nf-cod-note"
                :face 'nerd-icons-blue
                :v-adjust -0.3)
       :function #'citar-has-notes
       :padding "    "
       :tag "has:notes"))
    (defvar citar-indicator-cited-icons
      (citar-indicator-create
       :symbol (nerd-icons-faicon
                "nf-fa-circle_o"
                :face 'nerd-icon-green)
       :function #'citar-is-cited
       :padding "  "
       :tag "is:cited"))
    (setq citar-indicators
       (list citar-indicator-files-icons
                citar-indicator-links-icons
                citar-indicator-notes-icons
                citar-indicator-cited-icons)))

(map! :leader
      (:prefix ("l" . "literature")
         :desc "Insert Citation"        "@" #'citar-insert-citation
         :desc "Attach Files"           "a" #'citar-attach-files
         :desc "Open Files"             "f" #'citar-open-files
         :desc "Insert Citation"        "i" #'citar-insert-citation
         :desc "Insert Citekey"         "I" #'citar-insert-keys
         :desc "Open Notes"             "n" #'citar-open-notes
         :desc "Open Existing Note"     "N" #'org-roam-ref-find
         :desc "Open"                   "o" #'citar-open
         :desc "Insert Reference"       "r" #'citar-insert-reference))
(map! :localleader :map evil-tex-mode-map :desc "Insert quick citation" "@"
        (lambda () (interactive) (let ((current-prefix-arg '(4))) ; call with C-u prefix argument
                                   (call-interactively #'citar-insert-citation))))

(map! :after pdf-tools :localleader :map pdf-view-mode-map
      :desc "auto slice mode" "s" 'pdf-view-auto-slice-minor-mode
      :desc "midnight mode" "m" 'pdf-view-midnight-minor-mode
      :desc "themed mode" "t" 'pdf-view-themed-minor-mode
      :desc "printer mode" "p" 'pdf-view-printer-minor-mode
      (:prefix ("f" . "fit")
         :desc "fit page to window"     "p" #'pdf-view-fit-page-to-window
         :desc "fit width to window"    "w" #'pdf-view-fit-width-to-window
         :desc "fit height to window"   "h" #'pdf-view-fit-height-to-window))

(map! :after pdf-tools :map pdf-view-mode-map
      "<normal-state> C-f" 'pdf-view-next-page-command
      "<normal-state> C-b" 'pdf-view-previous-page-command
      :desc "midnight mode" "m" 'pdf-view-midnight-minor-mode
      ;; free up window navigation keys
      "C-j" nil
      "C-l" nil
      "C-k" nil
      "<normal-state> C-j" nil
      "<normal-state> C-l" nil
      "<normal-state> C-k" nil
      ;; org-noter keybindings
      "<normal-state> <remap> <evil-insert>" nil
      "<normal-state> i" 'org-noter-insert-note
      "i" 'org-noter-insert-note
      "<normal-state> <remap> <evil-insert-line>" nil
      "<normal-state> I" 'org-noter-insert-precise-note
      "I" 'org-noter-insert-precise-note)

(setq pdf-view-midnight-colors '("#E4E6EB" . "#18191A"))

(add-hook! 'pdf-view-mode-hook :append #'pdf-view-auto-slice-minor-mode #'pdf-view-themed-minor-mode #'pdf-view-fit-width-to-window)

(after! org-noter
  (org-noter-enable-org-roam-integration))
(setq! org-noter-always-create-frame nil
       org-noter-kill-frame-at-session-end nil
       org-noter-prefer-root-as-file-level t)

(map! :after org :localleader :map org-mode-map
      "N" 'org-noter)
