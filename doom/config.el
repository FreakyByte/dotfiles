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

(setq emacs-everywhere-frame-name-format "emacs-everywhere")

(remove-hook 'emacs-everywhere-init-hooks #'emacs-everywhere-set-frame-position)

(setq emacs-everywhere-major-mode-function #'org-mode)

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

(after! doom-themes
        (custom-theme-set-faces! doom-theme
          `(default :background ,(ewal-load-color 'background))
          `(hl-line :background ,(ewal--color-chshade
                        (ewal-load-color 'background) .1))
          `(org-block :background ,(ewal--color-chshade
                      (ewal-load-color 'background) -0.3))

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

(setq-default fill-column 100)

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

(map! :leader
 (:prefix ("t" . "toggle")
       :desc "transparency"          "t"     #'toggle-background-opacity
       )
      )

(setq doom-frame-opacity 100)
(toggle-background-opacity)

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

(setq company-idle-delay 0.4)

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

(map! :leader
      (:prefix ("y" . "YASnippet")
       :desc "edit snippet" "e" #'yas-visit-snippet-file
       :desc "insert snippet" "i" #'yas-insert-snippet
       :desc "new snippet" "n" #'+snippets/new
       :desc "find private snippet" "p" #'+snippets/find-private
       )
      )

(map! :leader
 (:prefix ("t" . "toggle")
       :desc "Global writeroom mode"  "W"     #'global-writeroom-mode
       )
      )

(setq org-directory "~/org/"
      org-roam-directory "~/Dropbox/roam"
      org-cd-directory (concat org-roam-directory "/tikz-cd")) ; for commutative diagrams
(setq org-agenda-files (list "~/org/todo.org" "~/org/lv_Sommer2023.org"))

(after! org
  (setq org-ellipsis " ▼ "
        ;;org-superstar-headline-bullets-list '("◉" "●" "○" "◆" "●" "○" "◆")
        org-superstar-headline-bullets-list '("❭")
        org-superstar-item-bullet-alist '((?+ . ?✦) (?- . ?➤)) ; changes +/- symbols in item lists
        org-hide-emphasis-markers t     ; do not show e.g. the asterisks when writing something in boldface
        org-appear-autoemphasis t
        org-appear-autosubmarkers t
        org-appear-autolinks nil
        org-hidden-keywords '(title)  ; hide #+TITLE:
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
  `(org-document-title :family "K2D" :foreground "#9BDB4D" :background nil :height 2.0)
)

(setq org-roam-capture-templates
      '(("d" "default" plain "%?" :target
            (file+head "%<%Y%m%d%H%M%S>-${slug}.org" "#+filetags: \n#+title: ${title}\n\n")
        :unnarrowed t)))

(defun jethro/tag-new-node-as-draft ()
  (org-roam-tag-add '("draft")))
(add-hook 'org-roam-capture-new-node-hook #'jethro/tag-new-node-as-draft)

(defun org-roam-node-insert-immediate (arg &rest args)
  (interactive "P")
  (let ((args (cons arg args))
        (org-roam-capture-templates (list (append (car org-roam-capture-templates)
                                                  '(:immediate-finish t) ; this is the essential bit
                                                  ))))
    (apply #'org-roam-node-insert args)))

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
          org-roam-ui-open-on-start t))

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
         :desc "Open random node"           "a" #'org-roam-node-random
         :desc "Find node"                  "f" #'org-roam-node-find
         :desc "Find ref"                   "F" #'org-roam-ref-find
         :desc "Show UI"                    "g" #'org-roam-ui-open
         :desc "Insert node"                "i" #'org-roam-node-insert
         :desc "Insert node immediately"    "I" #'org-roam-node-insert-immediate
         :desc "Capture to node"            "n" #'org-roam-capture
         :desc "Toggle roam buffer"         "r" #'org-roam-buffer-toggle
         :desc "Launch roam buffer"         "R" #'org-roam-buffer-display-dedicated
         :desc "Sync database"              "s" #'org-roam-db-sync
         :desc "Add tag"                    "t" #'org-roam-tag-add
         :desc "Remove tag"                 "T" #'org-roam-tag-remove
         :desc "Commutative diagram"        "c" #'org-capture-commutative-diagram
         (:prefix ("d" . "by date")
          :desc "Goto previous note"        "b" #'org-roam-dailies-goto-previous-note
          :desc "Goto date"                 "d" #'org-roam-dailies-goto-date
          :desc "Capture date"              "D" #'org-roam-dailies-capture-date
          :desc "Goto next note"            "f" #'org-roam-dailies-goto-next-note
          :desc "Goto tomorrow"             "m" #'org-roam-dailies-goto-tomorrow
          :desc "Capture tomorrow"          "M" #'org-roam-dailies-capture-tomorrow
          :desc "Capture today"             "n" #'org-roam-dailies-capture-today
          :desc "Goto today"                "t" #'org-roam-dailies-goto-today
          :desc "Capture today"             "T" #'org-roam-dailies-capture-today
          :desc "Goto yesterday"            "y" #'org-roam-dailies-goto-yesterday
          :desc "Capture yesterday"         "Y" #'org-roam-dailies-capture-yesterday
          :desc "Find directory"            "-" #'org-roam-dailies-find-directory)))

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

(defun roam-pseudohook ()
  (cond ((string-prefix-p org-roam-directory (buffer-file-name))
         (window-margin-mode 1)
         (mixed-pitch-mode 1)
         )))
(after! org (add-hook 'org-mode-hook 'roam-pseudohook))

(defun writeroom-mode-deactivate () (writeroom-mode -1))
(add-hook 'org-roam-capture-new-node-hook 'writeroom-mode-deactivate)
(add-hook 'org-capture-mode-hook 'writeroom-mode-deactivate)

(setq org-roam-node-display-template
      (concat "${title:*} "
              (propertize "${tags:30}" 'face 'org-tag))) ; 30 is the max. number of characters allocated for tags

(after! org (setq org-format-latex-header (concat org-format-latex-header
    "\\usepackage{tikz, pgfplots}\\pgfplotsset{compat=1.16}\\usetikzlibrary{cd}")))

(after! org (setq org-latex-create-formula-image-program 'imagemagick))

(after! org (setq org-startup-with-latex-preview t))
(use-package! org-fragtog
    :after org
    :hook (org-mode . org-fragtog-mode) ; this auto-enables it when you enter an org-buffer
    :config
)

(require 'org-src)
(add-to-list 'org-src-block-faces '("latex" (:inherit default :extend t)))

(setq org-latex-default-scale 1.5)
(setq org-latex-writeroom-scale 2.5)
(setq org-latex-big-font-scale 2.5)

(defun org-latex-preview-clear ()
  "Disable org-latex-preview (which is the same as running org-latex-preview with prefix argument)"
  (interactive)
  (let ((current-prefix-arg '(4)))
    (call-interactively 'org-latex-preview)))

(defun latex-preview-rescale ()
  (cond ((bound-and-true-p writeroom-mode) (setq org-format-latex-options (plist-put org-format-latex-options :scale org-latex-writeroom-scale)))
        ((bound-and-true-p doom-big-font-mode) (setq org-format-latex-options (plist-put org-format-latex-options :scale org-latex-big-font-scale)))
        (t (setq org-format-latex-options (plist-put org-format-latex-options :scale org-latex-default-scale)))
    )
  ;; re-render LaTeX fragments
  (org-latex-preview-clear)
  (org-latex-preview)
  )
(add-hook 'writeroom-mode-hook 'latex-preview-rescale)
(add-hook 'doom-big-font-mode-hook 'latex-preview-rescale)

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

(add-hook 'TeX-mode-hook 'window-margin-mode)

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
