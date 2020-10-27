;; Example user config, copy to lesser-evil-user-config.el and modify to your needs.

;; enable use of emacsclient
(server-start)

;; Set the default font
(set-frame-font "Iosevka Fixed SS14-14")

;; Show line numbers
(global-linum-mode 1)

;; Delete trailing whitespace when saving code files
(add-hook 'before-save-hook
          (lambda ()
            (when (derived-mode-p 'prog-mode)
              (delete-trailing-whitespace))))

;; Restore open buffers/files when starting Emacs
(desktop-save-mode)

;; Set which binding files to use, later files in the list take precedence over
;; earlier ones. File names are expanded based on user-emacs-directory (use
;; absolute file names if you don't want that)
(setq eml/bindings-files (list "leader-bindings.el"
                               "my-bindings.el"))
