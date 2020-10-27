;; Example user config, copy to lesser-evil-user-config.el and modify to your needs.

(server-start)

(set-frame-font "Iosevka Fixed SS14-14")

(global-linum-mode 1)

(add-hook 'before-save-hook
          (lambda ()
            (when (derived-mode-p 'prog-mode)
              (delete-trailing-whitespace))))

(desktop-save-mode)
