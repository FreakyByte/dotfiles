;;; .dir_locals.el dotfiles  -*- lexical-binding: t; -*-

;; settings for org hugo
((org-mode . ( ;; settings applying to org files
        ;; figure out the this file is in to avoid absolute file paths
        (eval . (set (make-local-variable 'this-project-path)
                    ;; note: this might not work when using more .dir-locals.el files in subdirectories
                    (locate-dominating-file default-directory ".dir-locals.el")))
        (eval . (setq org-hugo-base-dir (concat this-project-path "docs")))
        (eval . (setq org-hugo-section ""))
        (eval . (org-hugo-auto-export-mode))
    ))
)
