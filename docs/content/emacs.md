+++
title = "DOOM Emacs Config"
author = ["Michael Reitmeir"]
draft = false
weight = 2
+++

## About this Config {#about-this-config}

Here is some help from the default config.el that I haven't found a better spot for yet.

> Here are some additional functions/macros that could help you configure Doom:
>
> -   \`load!' for loading external \*.el files relative to this one
> -   \`use-package!' for configuring packages
> -   \`after!' for running code after a package has loaded
> -   \`add-load-path!' for adding directories to the \`load-path', relative to
>     this file. Emacs searches the \`load-path' when you load packages with
>     \`require' or \`use-package'.
> -   \`map!' for binding new keys
>
> To get information about any of these functions/macros, move the cursor over
> the highlighted symbol at press 'K' (non-evil users must press 'C-c c k').
> This will open documentation for it, including demos of how they are used.
>
> You can also try 'gd' (or 'C-c c d') to jump to their definition and see how
> they are implemented.


## Basics {#basics}

```emacs-lisp
;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

(setq user-full-name "Michael Reitmeir"
      user-mail-address "michi.reitmeir@gmail.com")
```


### Sensible Defaults {#sensible-defaults}

Inspired by [tecosaur](https://tecosaur.github.io/emacs-config/config.html#better-defaults), who was in turn inspired by others. Funny how that works.

```emacs-lisp
(setq-default
 delete-by-moving-to-trash t                      ; Delete files to trash
 window-combination-resize t                      ; take new window space from all other windows (not just current)
 x-stretch-cursor t)                              ; Stretch cursor to the glyph width

(setq undo-limit 80000000                         ; Raise undo-limit to 80Mb
      evil-want-fine-undo t                       ; By default while in insert all changes are one big blob. Be more granular
      auto-save-default t                         ; Nobody likes to loose work, I certainly don't
      truncate-string-ellipsis "…")

(setq evil-vsplit-window-right t
      evil-split-window-below t)

(defconst doom-module-config-file (concat doom-user-dir "config.org"))
(map! :leader
 (:prefix ("o" . "open")
       :desc "Emacs config"     "c"     #'doom/goto-private-config-file))
```

The [fish](https://fishshell.com/) shell is not [POSIX compatible](https://stackoverflow.com/questions/48732986/why-how-fish-does-not-support-posix), which among other things causes a bunch of garbage characters to appear whenever it is used in emacs (at least with my fish config). So it's better to have emacs use bash instead, especially in the interest of packages that rely on shell outputs

```emacs-lisp
(setq shell-file-name (executable-find "bash"))
```


### Emacs EVERYWHERE {#emacs-everywhere}

This changes the name of windows created with `emacs-everwhere`, so that I can more easily customize their behaviour in my window manager.

```emacs-lisp
(setq emacs-everywhere-frame-name-format "emacs-everywhere")
```

Specifically, I want my window manager to handle it's position rather than emacs.

```emacs-lisp
(remove-hook 'emacs-everywhere-init-hooks #'emacs-everywhere-set-frame-position)
```

Org mode is nice. But the variable `emacs-everywhere-major-mode-function` itself doesn't seem to do the trick. So let's add org to the init hook as well.

```emacs-lisp
(setq emacs-everywhere-major-mode-function #'org-mode)
(add-hook 'emacs-everywhere-init-hooks 'org-mode)
```


## Appearance {#appearance}


### Fonts {#fonts}

From the default config.el:

> Doom exposes five (optional) variables for controlling fonts in Doom. Here
> are the three important ones:
>
> -   \`doom-font'
> -   \`doom-variable-pitch-font'
> -   \`doom-big-font' -- used for \`doom-big-font-mode'; use this for
>     presentations or streaming.
>
> They all accept either a font-spec, font string ("Input Mono-12"), or xlfd
> font string. You generally only need these two:
> (setq doom-font (font-spec :family "monospace" :size 12 :weight 'semi-light)
>       doom-variable-pitch-font (font-spec :family "sans" :size 13))

```emacs-lisp
(setq doom-font (font-spec :family "JetBrains Mono" :size 15 :weight 'regular)
      doom-variable-pitch-font (font-spec :family "Libertinus Sans" :size 19))
;;      doom-variable-pitch-font (font-spec :family "Fira Sans") ; inherits `doom-font''s :size
;;      doom-unicode-font (font-spec :family "Input Mono Narrow" :size 12)
;;      doom-big-font (font-spec :family "Fira Mono" :size 19))

(setq-default line-spacing 0.25)
```

I also gotta activate `mixed-pitch-set-height`, so that `mixed-pitch-mode` actually uses the `:size` option from the variable pitch font above.

```emacs-lisp
(setq mixed-pitch-set-height t)
```

Let's bind some keys for quick toggling between the different font modes.

```emacs-lisp
(map! :leader
 (:prefix ("t" . "toggle")
       :desc "Mixed pitch mode"       "m"     #'mixed-pitch-mode
       :desc "Variable pitch mode"    "v"     #'variable-pitch-mode
       )
      )
```

By this, `visible-mode` has been unmapped from `SPC t v`. Let's remap it to `SPC t V`.

```emacs-lisp
(map! :leader
 (:prefix ("t" . "toggle")
       :desc "Visible mode"           "V"     #'visible-mode
       )
      )
```


### Theme {#theme}

I like DOOMs default colors

```emacs-lisp
(setq doom-theme 'doom-one)
```

But I still wanna customize it a bit.


#### Shades with ewal {#shades-with-ewal}

I use [pywal](https://github.com/dylanaraps/pywal) to have the colors in my terminal fit my wallpaper.
While for Emacs I use the colors from the `doom-one` theme, I still want the (transparent) backgrounds of different parts of Emacs to synergize with pywal.
For this, the [ewal](https://github.com/cyruseuros/ewal) package is used to get the color scheme from pywal into Emacs.

The faces and their desired changes are stored in `ewal-background-list`. The function `toggle-ewal-background` then loops over this list and applies the changes, unless `ewal-background-active` is non-nil, in which case it reloads the theme to undo the changes.

```emacs-lisp
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
```

Let's turn on the ewal background by default.

```emacs-lisp
(after! (doom-themes org hl-line)
        (toggle-ewal-background))
```


### Modeline {#modeline}

Making the modeline a bit taller than the default height (25) seems nice.

```emacs-lisp
(setq doom-modeline-height 35)
```


### Dashboard (startup page) {#dashboard--startup-page}

I like this silly banner I found at <https://github.com/jeetelongname/doom-banners> for my dashboard. To fit with this, font color is changed to pink.

```emacs-lisp
(after! doom-themes
    (custom-theme-set-faces! 'doom-one
        `(doom-dashboard-banner :foreground "pink" :weight bold)))
(setq fancy-splash-image "~/.config/doom/I-am-doom.png")
(setq +doom-dashboard-banner-padding '(0 . 4))
```

Now let's change the menu options on the dashboard. I added `doom-dashboard-widget-projects`, which contains extra buttons for all projects known to projectile. The source code is adapted from the `doom-dashboard-widget-shortmenu` function.

```emacs-lisp
(setq +doom-dashboard-functions
      '(doom-dashboard-widget-banner
        doom-dashboard-widget-shortmenu
        doom-dashboard-widget-projects))

(setq +doom-dashboard-menu-sections
 '(("Configure Emacs"
    :icon (concat (nerd-icons-icon-for-mode 'emacs-lisp-mode :face 'doom-dashboard-menu-title) " ")
    :action doom/goto-private-config-file
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
  "Create list of menu buttons for each project in `projectile-known-projects'."
        (dolist (project projectile-known-projects)
         (insert
          (+doom-dashboard--center (- +doom-dashboard--width 1)
           (format "%s %s\n"
            (nerd-icons-faicon "nf-fa-angle_right" :face 'doom-dashboard-project)
            (with-temp-buffer
             (insert-text-button project
                        'action `(lambda (_button) (projectile-switch-project-by-name ,project))
                        'face 'doom-dashboard-project
                        'follow-link t
                        'help-echo project)
             (format "%-41s" (buffer-string))))))))
```

The following is a hack to remove the blank lines between menu buttons. By investigating the last lines of the source code of `doom-dashboard-widget-shortmenu`, one can see that the newlines are inserted if `display-graphic-p` returns true. So let's temporarily overwrite this function with `ignore`, which always returns nil.

```emacs-lisp
(defadvice! no-new-lines (oldfun)
  :around #'doom-dashboard-widget-shortmenu
  (cl-letf (((symbol-function 'display-graphic-p) #'ignore))
    (funcall oldfun)))
```

Let's also add some shorter keybindings to the dashboard.

```emacs-lisp
(map! :map +doom-dashboard-mode-map
      :ng "c"       #'doom/goto-private-config-file
      :ng "r"       #'org-roam-node-find-default
      :ng "l"       #'citar-open-files
      :ng "p"       #'projectile-switch-project)
```


### Line Numbers, Wrapping and Margins {#line-numbers-wrapping-and-margins}

Display relative line numbers, but do so counting lines as displayed, not actual line breaks in the buffer.

```emacs-lisp
(setq display-line-numbers-type 'visual)
```

This works well for me, because I like overlength lines to always automatically wrap.

```emacs-lisp
(global-visual-line-mode t)
```

Maximum line length (when `word-wrap-mode` is active and `+word-wrap-fill-style` is set to `'auto` or `'soft`, or when `perfect-margin-mode` is active)

```emacs-lisp
(setq-default fill-column 110)
```

The [perfect-margin](https://github.com/mpwang/perfect-margin) package automatically centers windows if there is enough space for that. Keep in mind it needs to be installed with `(package! perfect-margin)` in `package.el`.

```emacs-lisp
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
```

However, in some modes the "perfect" margins don't make sense. The `writeroom-width` setting is overwritten by them, and with `doom-big-font-mode` there's simply not enough space. So let's filter those out. The dummy variable is there because `perfect-margin-ignore-filters` likes to call functions with the current window as parameter.

```emacs-lisp
(add-to-list 'perfect-margin-ignore-filters '(lambda (window) (bound-and-true-p writeroom-mode)))
(add-to-list 'perfect-margin-ignore-filters '(lambda (window) (bound-and-true-p doom-big-font-mode)))
```

Big Font Mode is actually even more resilient: It doesn't seem to let `perfect-margin-mode` deactivate itself properly while `doom-big-font-mode` is active. So some advice is necessary...

```emacs-lisp
(defadvice doom-big-font-mode (before deactivate-perfect-margins) (perfect-margin-mode 0))
```


### Zen/Writeroom {#zen-writeroom}

Zen mode (as it is called in doom emacs) or writeroom mode (the package it is based on) increases the font size, actives the mixed-pitch font and disables some possible distractions.

```emacs-lisp
(setq writeroom-width 45)
(map! :leader
 (:prefix ("t" . "toggle")
       :desc "Global writeroom mode"  "W"     #'global-writeroom-mode))
```


### Transparent Background {#transparent-background}

I like me some transparent backgrounds. This value controls the opacity if transparent background is enabled.

```emacs-lisp
(defconst frame-default-opacity 85)
```

In contrast, the variable `frame-opacity` is used for the current opacity. So this variable is set to `100` if transparency is disabled.

Now follows a function to toggle the transparent background on and off.

```emacs-lisp
(defvar opacity-type "background" "Type of opacity to use. If set to \"background\" only the background will be transparent. If set to \"full-frame\", the entire frame will be transparent. Needs to be refreshed using `update-background-opacity'")
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
```

The background transparency has been added to emacs only [somewhat recently](https://kristofferbalintona.me/posts/202206071000/). It doesn't work perfectly though, for example transparent PNG-images are not rendered as transparent, and it [doesn't look like this will be fixed soon](https://lists.gnu.org/archive/html/bug-gnu-emacs/2022-12/msg01080.html). So I use the `opacity-type` to be able to choose which kind of transparency I want.

This will make the background transparent at startup.

```emacs-lisp
(setq frame-opacity 100)
(toggle-frame-opacity)
```


### Whitespace {#whitespace}

Highlight unnecessary or wrong use of whitespace (e.g. mixed tabs and spaces).

```emacs-lisp
(use-package! whitespace
  :config (setq whitespace-style '(face empty indentation space-after-tab space-before-tab))
  (global-whitespace-mode +1))
```

Trailing whitespace doesn't need to be visualized, since it's removed on save anyway.


### Treemacs {#treemacs}

By default, the treemacs window is not re-sizable. I don't see why.

```emacs-lisp
(setq treemacs-width 30)
(setq treemacs--width-is-locked nil)
(setq treemacs-width-is-initially-locked nil)
```

Especially when using LaTeX, there's gonna be a lot of files in my directory which I don't actively care about. The following hides these files. (cf. [tecosaur](https://tecosaur.github.io/emacs-config/config.html#treemacs))

```emacs-lisp
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
```


## Global Functionality {#global-functionality}


### Local Leader {#local-leader}

I'm used to this from my VimTex days.

```emacs-lisp
(setq doom-localleader-key ",")
```


### Move/Cut {#move-cut}

I've always disliked that the delete command in vim automatically yanks the deleted text, i.e. it acts more like cutting than deleting.
For this reason I've configured 'd' and 'x' to not yank the deleted text, and instead defined 'm' (for "move", because 'c' is already taken) to delete and yank, i.e. cut.

First we clone the default `evil-delete` function under the name `evil-cut`.

```emacs-lisp
(setq wrapped-copy (symbol-function 'evil-delete))
(evil-define-operator evil-cut (BEG END TYPE REGISTER YANK-HANDLER)
  "Cut text from BEG to END with TYPE.

Save in REGISTER or in the kill-ring with YANK-HANDLER."
  (interactive "<R><x><y>")
  (funcall wrapped-copy BEG END TYPE REGISTER YANK-HANDLER))
```

Now we map `evil-cut` to 'm'.

```emacs-lisp
(map! :n "m" 'evil-cut)
```

Finally, we automatically redirect all deletions to the black hole register, thus making 'd', 'x', and pasting over something only delete and not copy.
We also need to do it for `evil-org-delete-char`, since that has different input arguments and an extra `evil-yank` in it's definition for some reason.

```emacs-lisp
(defun bb/evil-delete (orig-fn beg end &optional type _ &rest args)
  (apply orig-fn beg end type ?_ args))
(advice-add 'evil-delete :around 'bb/evil-delete)
(advice-add 'evil-delete-char :around 'bb/evil-delete)

(defun bb/evil-org-delete-char (orig-fn count beg end &optional type _ &rest args)
  (apply orig-fn count beg end type ?_ args))
(advice-add 'evil-org-delete-char :around 'bb/evil-org-delete-char)
```


### Windows and Buffers {#windows-and-buffers}

A key chord every time I want to switch windows or buffers is way too much work.

```emacs-lisp
(map! :n "ö" 'evil-next-buffer
      :n "Ö" 'evil-prev-buffer
      :n "C-ö" 'switch-to-buffer
      :n "C-j" 'evil-window-next
      :n "C-k" 'evil-window-prev
      :n "C-l" 'evil-window-vsplit
      :n "C-ä" 'evil-window-split)
(map! :after org
    :map org-mode-map
    "C-j" 'evil-window-next)
```

Tecosaur finds it handy to be asked which buffer to see after splitting a window. So do I. But let's tweak it so it only shows the buffers of the current workspace/perspective so I don't get overwhelmed. There's also no point in asking which buffer I want if there's only one available.

```emacs-lisp
(defadvice! prompt-for-buffer (&rest _)
  :after '(evil-window-split evil-window-vsplit)
  (if (<= (length (persp-buffer-list)) 1) nil (call-interactively 'persp-switch-to-buffer)))
```


### Auto-complete {#auto-complete}

Increase time until auto-complete shows up.

```emacs-lisp
(setq company-idle-delay 0.4)
```


### Search &amp; Replace {#search-and-replace}

Ok, I admit it: I never learned how to do search and replace properly in vim. It seems like too much to remember and type every time. So let's use emacs functions instead.
The `replace-string` and `query-replace` suit my needs the most, but by default they only operate from the current point to the end of the buffer, not on the whole buffer. This fixes that, while still working when used with a specific region selected (adapted from [here](https://superuser.com/a/1152391)).

```emacs-lisp
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
```


### Spell- and grammar checker {#spell-and-grammar-checker}

These are the dictionaries I want to use for spell checking.

```emacs-lisp
(add-hook 'spell-fu-mode-hook
  (lambda ()
    (spell-fu-dictionary-add (spell-fu-get-ispell-dictionary "de"))
    (spell-fu-dictionary-add (spell-fu-get-ispell-dictionary "en"))
    ))
(setq ispell-personal-dictionary "~/Dropbox/.aspell.en.pws")
```

Set path to languagetool.

```emacs-lisp
(setq langtool-java-classpath "/usr/share/languagetool/*")
```


### Snippets {#snippets}

My snippets are mostly to make typing LaTeX fast. I [disable Doom's default snippets](https://docs.doomemacs.org/v21.12/modules/editor/snippets/), but then I add some of them back again manually. Some other snippets come from this [excellent article by karthink](https://karthinks.com/software/latex-input-for-impatient-scholars/#create-math-environments).


#### Basic YAS settings {#basic-yas-settings}

This disables the annoying final newline when creating a snippet, which always screws things up.

```emacs-lisp
(add-hook 'snippet-mode-hook 'my-snippet-mode-hook)
(defun my-snippet-mode-hook ()
  "Custom behaviours for `snippet-mode'."
  (setq-local require-final-newline nil)
  (setq-local mode-require-final-newline nil))
```

Hey boy, I heard you like snippets... so I put some snippets in your snippets...

```emacs-lisp
(setq yas-triggers-in-field t)
```

Also I don't want to have to insert an unnecessary space before being able to expand a snippet.

```emacs-lisp
(setq yas-key-syntaxes '(yas-longest-key-from-whitespace "w_.()" "w_." "w_" "w"))
```

I use some snippets that modify the surrounding characters of the buffer (e.g. by deleting the space before the snippet). This causes YAS to throw a warning. Let's disable that.

```emacs-lisp
(use-package warnings
    :config
    (cl-pushnew '(yasnippet backquote-change)
                warning-suppress-types
                :test 'equal))
```


#### Keybindings (and avoiding remappings) {#keybindings--and-avoiding-remappings}

Some nicer shortcuts for creating snippets etc. would also be nice.
Doom however automatically remaps the functions `yas-new-snippet` and `yas-visit-snippet-file` to the "superior alternatives" `+snippets/new` and `+snippets/edit` (see [~/config/emacs/modules/editor/snippets/config.el, line 81](~/.config/emacs/modules/editor/snippets/config.el)). I respectfully disagree with that. My main problem is `+snippets/new` doesn't save the snippet in the right folder when creating a snippet for a mode different from the current major mode. Other issues are discussed [here](https://github.com/doomemacs/doomemacs/issues/4330). Since this issue is a bit older and hasn't seen recent activity, I'll opt for a workaround for now.

```emacs-lisp
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
```

Now let's just make `yas-new-snippet` use the same template as `+snippets/new`:

```emacs-lisp
  (setq yas-new-snippet-default (concat "# -*- mode: snippet -*-\n"
                                    "# name: $1\n"
                                    "# uuid: $2\n"
                                    "# key: $3\n"
                                    "# condition: ${4:t}\n"
                                    "# --\n"
                                    "$0"))
```


#### Automatic snippet expansion {#automatic-snippet-expansion}

YAS has no built-in way to auto-expand snippets, i.e. expand them without hitting tab. Another snippet engine, [AAS](https://github.com/ymarco/auto-activating-snippets), was made for this purpose. However, I prefer not dealing with two separate systems at the same time, so I opted for [manually adding auto expanding capabilities to YAS.](https://github.com/joaotavora/yasnippet/issues/998) This way, snippets that are marked with the condition `'auto` will be auto-expanded.

```emacs-lisp
  (defun yas-try-expanding-auto-snippets ()
    (when (and (boundp 'yas-minor-mode) yas-minor-mode)
      (let ((yas-buffer-local-condition ''(require-snippet-condition . auto)))
        (yas-expand))))
  (add-hook 'post-self-insert-hook #'yas-try-expanding-auto-snippets)
```


#### The TAB key {#the-tab-key}

The tab key is getting intentionally overloaded with snippets, cdlatex and various org-mode things. But one thing that annoyingly interferes with these is autocomplete. Lets unbind the tab key from that and rather use Return for autocomplete and arrow keys (or C-j, C-k) for picking completion suggestions.

```emacs-lisp
(after! company
        (map! :map company-search-map
                [tab] nil
                "TAB" nil)
        (map! :map company-active-map
                [tab] nil
                "TAB" nil))
```

Though sometimes cdlatex and YAS fight for whose turn it is with the tab key. This solves that (cf. [karthink](https://gist.github.com/karthink/7d89df35ee9b7ac0c93d0177b862dadb), adapted for doom).
(**TODO**: This makes default values in snippets harder to use. Hitting tab first jumps to the end of the field, and only hitting tab a second time jumps to the next field.)

```emacs-lisp
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
```


### Performance {#performance}

I am experiencing a bunch of little performance issues related to font-lock, so syntax highlighting and other visuals of text. One big one seems to be related to having many folded org headings on the screen, so should try to avoid that.

Another one comes in form of lags while typing "long" lines, where long is not actually long, but just a couple hundred characters. This setting delays font-lock for a bit, which seems to help

```emacs-lisp
(setq jit-lock-defer-time 0.25)
```


## Org Mode {#org-mode}


### Org-Paths {#org-paths}

```emacs-lisp
(setq ;org-directory "~/org/"
      org-roam-directory "~/Dropbox/roam"
      org-cd-directory (concat org-roam-directory "/tikz-cd")) ; for commutative diagrams
;;(setq org-agenda-files (list "~/org/todo.org" "~/org/lv_Sommer2023.org"))
(setq org-agenda-files nil) ;currently not using org-agenda
(setq org-directory nil) ;currently not using org-agenda
```


### Org Appearance {#org-appearance}

```emacs-lisp
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
```


### Org Roam {#org-roam}

A good resource to read for some org-roam configuration goodness is [this guide from the creator of org-roam themselves.](https://jethrokuan.github.io/org-roam-guide/)


#### Capture {#capture}

My default template for regular notes:

```emacs-lisp
(setq org-roam-default-template '("d" "default" plain "%?" :target
            (file+head "%<%Y%m%d%H%M%S>-${slug}.org" "#+filetags: :draft:\n#+title: ${title}\n\n")
        :unnarrowed t :immediate-finish t))
```

Notice:

-   I want nodes to be marked as a draft, until I revisit and refine them. (I originally used the method described [here](https://jethrokuan.github.io/org-roam-guide/) for this, but I personally only want the draft tag on the default template, not on other templates.)
-   When I insert a link to a note that doesn't exist yet, I don't wanna be interrupted and type stuff in that new note before returning to the original one. I also find capture buffers a bit annoying sometimes and would prefer to just start typing in a regular org buffer. The `:immediate-finish` keyword takes care of both of that, when calling `org-roam-node-insert` and `org-roam-node-find`, respectively. If I really want a capture buffer, I can still use `org-roam-capture`, as this overwrites `:immediate-finish` anyways. (If you only want captures to sometimes finish immediately, the approach presented by [SystemCrafters here](https://systemcrafters.net/build-a-second-brain-in-emacs/5-org-roam-hacks/) is nice.)

Now we make the list of templates. For now it just contains the default template, more are to come later on.

```emacs-lisp
(setq org-roam-capture-templates (list org-roam-default-template))
```

The default template is fast and simple, so most of the time, I want to skip the template selection buffer and use just the default.

```emacs-lisp
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
```

Additionally, I'm getting quite annoyed that links are inserted at the cursor position, not after the cursor position. Even though I guess this is consistent with usual vim functionality, having to press space twice feels weird to me, and binding a new key to have it work similar to "append" ('a' in vim) rather than "insert" ('i' in vim) is a bit unnecessary. So this workaround suits me best. (cf. [this issue](https://github.com/syl20bnr/spacemacs/issues/14137))

```emacs-lisp
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
```


#### org-roam-ui {#org-roam-ui}

One of the killer features associated with org roam are fancy graphs, as e.g. provided by `org-roam-ui`.

```emacs-lisp
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
```

There seems to be a bug in `org-roam-ui` that it only shows the first tag of a node when using emacs 29s `sqlite-builtin` (see [this issue](https://github.com/org-roam/org-roam-ui/issues/289)). So I unfortunately have to switch to using the system wide sqlite.

```emacs-lisp
(setq org-roam-database-connector 'sqlite)
```


#### Tagging links {#tagging-links}

While org-roam allows for files to be tagged, the links between files cannot. The following adds this functionality. Furthermore, I'm using a [fork of org-roam-ui](https://github.com/odomanov/org-roam-ui) which allows filtering these link tags and assigning colors to them (see `packages.el`). This whole issue is talked about at length in the org-roam discourse (e.g. [here](https://org-roam.discourse.group/t/link-categorization/2486/3) and [here](https://org-roam.discourse.group/t/add-link-tags-feature/171/34)), but the discussion that this code comes from is [here](https://github.com/org-roam/org-roam-ui/discussions/25) (specifically this [gist here](https://gist.github.com/odomanov/ed070a7faf3df1377fccf5d7c5000bf8)).

The syntax for these tagged links is
`[[<link id>|:tag <tag> :context <short description>][<link title>]]`.
Beware that this is different from what the author explains in the discussion linked above! It seems to be `:tag`, not `:tags`!

```emacs-lisp
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
```

My main use case for this are links tagged with "implication". I use those when one mathematical property implies another. As a simple example, if my roam database had the nodes "rational number" and "real number", then the former should have a link to the latter tagged with "implication", since every rational number is a real number. Then I can filter for links tagged as implications in `org-roam-ui` to see how different mathematical structures relate to each other.

To make all this nice to use, let's write a bunch of functions to add and remove link tags. These are mostly based on the function `org-roam-link-replace-at-point`.

```emacs-lisp
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
```


#### Commutative Diagrams {#commutative-diagrams}

I want to use commutative diagrams in some of my roam notes, using the LaTeX-package `tikz-cd`. However, doing that in LaTeX fragments doesn't work with `org-roam-ui` (since that just uses KaTeX, which doesn't support everything in LaTeX). On the other hand, doing it using src-blocks is also not great, cause then the distracting source code is gonna appear both in org and in the UI.

My solution to that is creating a capture template for commutative diagrams (inspired by [this](https://github.com/darknmt/org-tikzcd-snippet)). This is done using regular `org-capture`, since I don't want those files to have IDs and show up in my roam database. This works as follows:

-   Upon running `org-capture-commutative-diagram`, the user is first prompted for a file name.
-   Afterwards, an org file is created, where the file name is prefixed with a time stamp. The file already contains a `tikz-cd` block, and all options necessary for export.
-   Now the user may type the commutative diagram of their dreams.
-   After completing the capture with `C-c C-c`, the diagram will be rendered to a png image by LaTeX/imagemagick. After this is finished, the capture buffer closes, and a link to the image is inserted in the previously opened buffer.

For the actual capture template: The code is passed both through `format` and through `org-capture`, which necessitates double escaping quotations and backspaces. This makes it super hard to read, so I suggest you just try it out if you wanna see what it does. ^^ The `%%` is a masked percentage sign btw.
Also there are checks in place to make sure the functions place in `org-capture`-hooks are only run when actually creating a commutative diagram (cf. [stackexchange](https://emacs.stackexchange.com/a/48567)).

```emacs-lisp
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
```


#### Keybindings {#keybindings}

Basically taking the default keybindings and moving them to `SPC r`, which was still free.
Only change is that I'm using `org-roam-ui` for the graph.

```emacs-lisp
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
```

Then additionally, I want quick control over the UI from the local leader.

```emacs-lisp
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
```


#### Roam Pseudohook {#roam-pseudohook}

It'd be nice to be able to toggle some settings only for roam notes. Usually this would be done via the hook of a mode. But roam notes are just org files, and I don't want those settings on all org files. So let's create something I'll call a "pseudohook". The function will run the hook if the current buffer file name is in `org-roam-directory`. By adding this function to `org-mode-hook`, the `roam-pseudohook` will apply exactly to the org files in `org-roam-directory`.

```emacs-lisp
(defvar roam-pseudohook nil
 "A hook run only on org files in org-roam-directory.")
(defun roam-pseudohook-function ()
  (cond ((string-prefix-p org-roam-directory (buffer-file-name))
         (run-hooks 'roam-pseudohook)
         )))
(after! org (add-hook 'org-mode-hook 'roam-pseudohook-function))
```


#### Appearance {#appearance}

I want org roam notes to have special appearance.

```emacs-lisp
(add-hook 'roam-pseudohook (lambda () (setq-local +word-wrap-fill-style 'soft) (+word-wrap-mode 1)))
(add-hook 'roam-pseudohook (lambda () (mixed-pitch-mode 1)))
```

Writeroom mode isn't a great idea during capture buffers. Let's add a hook to `org-capture-mode` to disable it.

```emacs-lisp
(defun writeroom-mode-deactivate () (writeroom-mode -1))
(add-hook 'org-roam-capture-new-node-hook 'writeroom-mode-deactivate)
(add-hook 'org-capture-mode-hook 'writeroom-mode-deactivate)
```

I want to see my tags when searching for notes.

```emacs-lisp
(setq org-roam-node-display-template
      (concat "${title:*} "
              (propertize "${tags:30}" 'face 'org-tag))) ; 30 is the max. number of characters allocated for tags
```


### Org LaTeX previews {#org-latex-previews}


#### The fancy new system™ {#the-fancy-new-system}

There is a fancy new LaTeX preview system underway. It is being developed by [Karthink](https://karthinks.com/) and [Tecosaur](https://tecosaur.github.io/emacs-config/config.html). It makes rendering previews asynchronous (meaning emacs doesn't need to wait until they're done) and really fast. It also makes some improvements on appearance, such as previews scaling with font size and being properly aligned with the surrounding text. More info [here](https://abode.karthinks.com/org-latex-preview/).

Currently, the patch of org mode that contains this fancy system has to be installed manually. It is kinda tedious, more info on the page linked above. The big issue is that often there will still be some remains of the old org version loaded, resulting in a mixed installation that makes nothing work. For me it was necessary to

-   completely reinstall doom without anything org related (so all modules that load org in `init.el` commented out) to really get rid of the old version of org,
-   then install the new patch
-   and only then install all other org related packages.
-   I also needed a new full TeXLive installation (to have packages like [mylatexformat](https://ctan.org/pkg/mylatexformat?lang=de)) and rebuild emacs with svg support enabled.

Thankfully, it is planned to have the new system merged into org mode itself, which will get rid of all of this messy installation.

To check whether everything is correctly installed, there's this neat function:

```emacs-lisp
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
```


#### Basic settings &amp; preamble {#basic-settings-and-preamble}

The following are some basic settings for this system, including the latex packages that are supposed to be loaded. Keep in mind that most of this will not work if you don't use the new system mentioned above!

```emacs-lisp
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
```

While `org-latex-preview-auto-mode` works pretty great, it doesn't automatically render fragments when I open a new buffer. I want that at least for my roam notes.
Annoyingly, the default method of rendering all previews in a buffer is by running `org-latex-preview` with a prefix argument (i.e. by pressing `C-u` before running the function). Let's make explicit functions for it instead.

```emacs-lisp
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
```


#### LaTeX Macros {#latex-macros}

Typing `\operatorname` is very annoying, even with cdlatex. So lets declare a bunch of macros and add them to both the latex previews and org-roam-ui.

```emacs-lisp
(setq org-latex-mathoperators (list
        "acl" "Ad" "Aut" "bd" "card" "cl" "coker" "Covar" "dcl" "ded" "ED" "End" "Ext" "fr" "Frac" "GL" "Hom" "id" "im" "ind" "lexmin" "lexmax" "Li" "Mat" "ord" "RM" "sinc" "SL" "SO" "Spec" "st" "Sub" "Th" "tp" "Tor" "Var"))
(dolist (macro org-latex-mathoperators)
  (setq org-latex-preview-preamble (concat org-latex-preview-preamble "\\DeclareMathOperator{\\" macro "}{" macro "}"))
  (add-to-list 'org-roam-ui-latex-macros (cons (concat "\\" macro) (concat "\\operatorname{" macro "}")) t)
  )
```


#### Settings necessary for TikZ (DISABLED) {#settings-necessary-for-tikz--disabled}

Not necessary since I now use svg rendering

There is two ways of rendering inline LaTeX previews: `dvipng` and `imagemagick`.
TikZ (and in particular `tikzcd`) don't like `dvipng` somehow. So let's switch over to the magicks:

```emacs-lisp
(after! org (setq org-latex-create-formula-image-program 'imagemagick))
```

For this to work however, `imagemagick` needs some further customization outside of Emacs (see [here](https://stackoverflow.com/a/59193253)).


#### org-fragtog (DISABLED) {#org-fragtog--disabled}

The `org-fragtog` package then enables automatically switching between LaTeX-preview and its underlying code. It is not necessary with the new fancy preview system, but I'll keep the code here for now.

```emacs-lisp
(after! org (setq org-startup-with-latex-preview t))
(use-package! org-fragtog
    :after org
    :hook (org-mode . org-fragtog-mode) ; this auto-enables it when you enter an org-buffer
    :config
)
```


#### Correct Backgrounds {#correct-backgrounds}

The following makes sure the backgrounds of LaTeX fragments (or their surroundings) don't look bad (cf. [tecosaur](https://tecosaur.github.io/emacs-config/config.html#prettier-highlighting))

```emacs-lisp
(require 'org-src)
(add-to-list 'org-src-block-faces '("latex" (:inherit default :extend t)))
```


#### Automatically Update Size (DISABLED) {#automatically-update-size--disabled}

I made my own primitive system for this before getting the new fancy preview system, which does this much better. I'll still keep the code and text here for now though.

The size of LaTeX fragments does not automatically update when the font size is changed. This fixes that.
It turned out to not be so easy though for an elisp noob like me, so here are some notes:

-   Annoyingly, disabling LaTeX previews is achieved by running `org-latex-preview` with _prefix argument_, i.e. by pressing `C-u` before running the function. Calling this from a script is a bit of a hassle. This is what happens in `org-latex-preview-clear`.
-   I want to check whether writeroom-mode is active. This is done by checking the `writeroom-mode` variable. However, this variable is at the same time the function that toggles the mode. So `bound-and-true-p` is used to only check the variable and not call the function. (analogously for big-font-mode)

<!--listend-->

```emacs-lisp
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
```


#### Smartparens {#smartparens}

I want Smartparens to also recognize typical LaTeX-patterns in org-mode (cf. [stackexchange](https://emacs.stackexchange.com/a/56094)).

```emacs-lisp
(require 'smartparens-config)
  (sp-local-pair 'org-mode "\\[" "\\]")
  (sp-local-pair 'org-mode "$" "$")
  (sp-local-pair 'org-mode "'" "'" :actions '(rem))
  (sp-local-pair 'org-mode "=" "=" :actions '(rem))
  (sp-local-pair 'org-mode "\\left(" "\\right)" :trigger "\\l(" :post-handlers '(sp-latex-insert-spaces-inside-pair))
  (sp-local-pair 'org-mode "\\left[" "\\right]" :trigger "\\l[" :post-handlers '(sp-latex-insert-spaces-inside-pair))
  (sp-local-pair 'org-mode "\\left\\{" "\\right\\}" :trigger "\\l{" :post-handlers '(sp-latex-insert-spaces-inside-pair))
  (sp-local-pair 'org-mode "\\left|" "\\right|" :trigger "\\l|" :post-handlers '(sp-latex-insert-spaces-inside-pair))
```


### Xournal++ integration ("Org Notebook") {#xournal-plus-plus-integration--org-notebook}

I take handwritten notes using [Xournal++](https://github.com/xournalpp/xournalpp). Being able to integrate those into org files sounds great, especially for hand-drawn diagrams.
There exists a package for this called [org-xournalpp](https://gitlab.com/vherrmann/org-xournalpp), but the following [code snippet I found on reddit](https://www.reddit.com/r/orgmode/comments/egasgy/comment/fc5molm/) works better for my use case. Why? Because it directly links images into the org document, which also works with `org-roam-ui`.
I modified the snippet slightly to also work in files without headings, work with a template, and export with transparent background.

```emacs-lisp
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
```


### org-d20 {#org-d20}

Org mode is really nice for tabletop RPGs, both taking notes as a player, as well as for writing your campaign as a game master.
The [org-d20](https://github.com/spwhitton/org-d20) minor mode allows for rolling dice and taking care of combat initiative and hp within org.

```emacs-lisp
(map! :localleader
      :map org-mode-map
      (:prefix ("D" . "org-d20")
       :desc "start/advance combat" "i" #'org-d20-initiative-dwim
       :desc "add to combat" "a" #'org-d20-initiative-add
       :desc "apply damage at point" "d" #'org-d20-damage
       :desc "roll" "r" #'org-d20-roll
       )
      )
```


## LaTeX {#latex}


### "Fixing" defaults {#fixing-defaults}

There are a couple of things that I, a person who learned LaTeX long before emacs, find quite annoying in how Doom is setup do deal with LaTeX by default.
These changes make everything feel more intuitive to me.

```emacs-lisp
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
       :desc "section"          "S"     #'evil-tex-toggle-section))
```

Always use latexmk.

```emacs-lisp
(add-hook 'LaTeX-mode-hook (lambda () (setq TeX-command-default "LaTeXMk")))
```

The following turns of all flycheck-warnings in AUCTex, since for the most part I just find them annoying.

```emacs-lisp
(setq flycheck-global-modes '(not LaTeX-mode latex-mode))
```

Unfortunately rainbow delimiters break frequently in LaTeX (because of "mismatched" delimiters in open intervals like `]a,b[` but also randomly at other times). Best to disable them.

```emacs-lisp
(add-hook 'TeX-mode-hook 'rainbow-delimiters-mode-disable
          'LaTeX-mode-hook 'rainbow-delimiters-mode-disable)
(after! latex
  (remove-hook 'TeX-update-style-hook #'rainbow-delimiters-mode))
```

Better shortcut for showing TeX-errors (backtick is very annoying on a German keyboard).

```emacs-lisp
(map! :localleader
      :map evil-tex-mode-map
      :desc "TeX-next-error"
      "e" #'TeX-next-error)
```

Another annoyance: I don't like it when AUCTex interferes with my quotation marks.
Removing this AUCTex-feature is reasonably simple and can either be done through the `TeX-quote-after-quote`-variable, or by just un-mapping `TeX-insert-quote` from the quotation mark key.
However, for whatever reason this same feature was also implemented in `smartparens`, specifically `smartparens-latex.el`. And removing this is _really_ a nightmare. None of the solutions I found online worked for me (see [here](https://github.com/doomemacs/doomemacs/issues/1688), [here](https://github.com/doomemacs/doomemacs/issues/485), [here](https://github.com/Fuco1/smartparens/issues/1100), [here](https://emacs.stackexchange.com/questions/34035/how-to-make-smartparens-insert-and-instead-of-in-latex-modes), [here](https://emacs.stackexchange.com/questions/31166/smartparens-not-insert-pair-of-latex-quotes?rq=1), [here](https://github.com/Fuco1/smartparens/issues/983), and [here](https://emacs.stackexchange.com/questions/52233/disable-tex-modes-auto-tex-insert-quote-functionaliy)...).
So instead, after like 3 hours of trial and error, I'm settling for this hack.

```emacs-lisp
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
```


### Viewer {#viewer}

Set default viewer to `pdf-tools` and automatically refresh the document buffer.

```emacs-lisp
(setq +latex-viewers '(pdf-tools zathura okular)
      TeX-view-program-selection '((output-pdf "Zathura") (output-pdf "Okular") (output-pdf "PDF Tools"))
      TeX-view-program-list '(("PDF Tools" TeX-pdf-tools-sync-view)
                              ("Okular" ("okular --noraise --unique file:%o" (mode-io-correlate "#src:%n%a")))
                              ("preview-pane" latex-preview-pane-mode))
      TeX-source-correlate-start-server t
      +latex-indent-item-continuation-offset 'auto)

;; Update PDF buffers after successful LaTeX runs
(add-hook 'TeX-after-compilation-finished-functions
           #'TeX-revert-document-buffer)
```


### Appearance {#appearance}

Long lines are hard to read. This activates a maximum line length in TeX-buffers.

```emacs-lisp
(add-hook 'TeX-mode-hook (lambda () (setq-local +word-wrap-fill-style 'soft) (+word-wrap-mode 1)))
```


### CDLaTeX {#cdlatex}

This mode provides pretty useful shortcuts for writing math. I ignored this package for too long, probably cause the default keybindings really don't work with me.

```emacs-lisp
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
```

The internal variables also need to be changed, or otherwise the old keys will still be active in the CDLaTeX menu.

```emacs-lisp
(require 'cdlatex)
(setq cdlatex-math-modify-prefix 180)
(setq cdlatex-math-symbol-prefix 96)
```

Let's also add a few more symbols/modifiers. (cf. [tecosaur](https://tecosaur.github.io/emacs-config/config.html#math-input-cdlatex))

```emacs-lisp
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
```


## Literature &amp; Citations {#literature-and-citations}

The `:biblio` module of Doom makes citations a lot easier. Built on [org-cite](https://blog.tecosaur.com/tmio/2021-07-31-citations.html), [citar](https://github.com/emacs-citar/citar), and [citar-org-roam](https://github.com/emacs-citar/citar-org-roam), it provides a uniform way of inserting citations in org-mode and LaTeX-mode, viewing saved PDFs and writing roam-notes on them.

In order to generate and maintain my bibliography, I'm using [Zotero](https://www.zotero.org/) (since there doesn't seem to be a solution that works fully within emacs, has comparable functionality and is as simple to set up). This automatically exports a BibLaTeX file (using [Better BibTeX](https://retorque.re/zotero-better-bibtex/)), which we should let emacs know about:

```emacs-lisp
(setq! citar-bibliography '("/home/reiti/Zotero/biblioteca.bib"))
(setq! org-cite-global-bibliography citar-bibliography)
```


### Note Taking {#note-taking}

Literature notes get their own subfolder.

```emacs-lisp
(setq citar-org-roam-subdir "/home/reiti/Dropbox/roam/literature")
```

The defaults for [citar-org-roam](https://github.com/emacs-citar/citar-org-roam) are pretty great already, I just want to modify the template a little. Let's start with the title:

```emacs-lisp
(setq citar-org-roam-note-title-template "${author} - ${title}")
```

Now the rest of the template. I want to have a link to the relevant pdf file in the note, if that exists. The intended way of achieving this is by extending `citar-org-roam-template-fields` to be able to automatically insert the file path of our reference like so:

```emacs-lisp
(setq citar-org-roam-template-fields '(
        (:citar-title "title")
        (:citar-author "author" "editor")
        (:citar-date "date" "year" "issued")
        (:citar-pages "pages")
        (:citar-type "=type=")
        (:citar-file "file" "pdf")))
```

Unfortunately, in case the `file`-entry of our bibliography entry is empty, this method will query the user for a filename (before evaluating any code that would eliminate empty strings). I don't want this, since I clearly don't have the file. So let's use `citar-get-value` instead and check for the empty sting that way.

```emacs-lisp
(add-to-list 'org-roam-capture-templates
  '("l" "Literature Note" plain
        "%?"
        :target
        (file+head
         "%(expand-file-name (or citar-org-roam-subdir \"\") org-roam-directory)/${citar-citekey}.org"
         ":PROPERTIES:\n:NOTER_DOCUMENT: %(if (string= \"\" \"%(citar-get-value \"file\" \"${citar-citekey}\")\") ( ) (print \"%(citar-get-value \"file\" \"${citar-citekey}\")\"))\n:NOTER_PAGE: 1\n:END:\n#+title: ${note-title}\n%(if (string= \"\" \"%(citar-get-value \"file\" \"${citar-citekey}\")\") (print \"${citar-citekey}\") (print \"[[file:%(citar-get-value \"file\" \"${citar-citekey}\")][${citar-citekey}]]\")), ${citar-date}\n\n")
        :unnarrowed t
     ) t)
```

Finally, we gotta tell `citar-org-roam` to use this template.

```emacs-lisp
(setq citar-org-roam-capture-template-key "l")
```


### Appearance {#appearance}

Even though the [citar documentation](https://github.com/emacs-citar/citar/wiki/Indicators) suggests otherwise, adding the `+icons` flag to dooms `:biblio` module doesn't do anything for me. So let's prettify it manually:

```emacs-lisp
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
```


### Keybindings {#keybindings}

```emacs-lisp
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
```


### PDF-Tools {#pdf-tools}

Some nicer keybindings.

```emacs-lisp
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
```

The doom one theme doesn't actually look too great for PDFs in my opinion. [This blog post](https://blog.karenying.com/posts/50-shades-of-dark-mode-gray) helped me pick something better:

```emacs-lisp
(setq pdf-view-midnight-colors '("#E4E6EB" . "#18191A"))
```

And now let's make everything behave the way I want from the get-go.

```emacs-lisp
(add-hook! 'pdf-view-mode-hook :append #'pdf-view-auto-slice-minor-mode #'pdf-view-themed-minor-mode #'pdf-view-fit-width-to-window)
```


### Org Noter {#org-noter}

```emacs-lisp
(after! org-noter
  (org-noter-enable-org-roam-integration))
(setq! org-noter-always-create-frame nil
       org-noter-kill-frame-at-session-end nil
       org-noter-prefer-root-as-file-level t)

(map! :after org :localleader :map org-mode-map
      "N" 'org-noter)
```


## Tangle this file! {#tangle-this-file}

Tangle on save? Reload after tangle? These hooks will ask you after every save.

;; Local Variables:
;; eval: (add-hook 'after-save-hook (lambda ()(if (y-or-n-p "Reload?")(doom/reload))) nil t)
;; eval: (add-hook 'after-save-hook (lambda ()(if (y-or-n-p "Tangle?")(org-babel-tangle))) nil t)
;; End:
