(add-to-list #'load-path (expand-file-name "init.d" user-emacs-directory))

(require 'better-defaults)
(require 'lesser-evil-commands)

(straight-use-package 'use-package)

(use-package ivy
  :straight t
  :defer 0.1
  :diminish
  :config (ivy-mode))

(use-package counsel
  :straight t
  :after (ivy)
  ;;:config (counsel-mode)
  )

(use-package swiper
  :straight t
  :after ivy)

(use-package undo-fu
  :straight t)

(use-package evil
  :straight t
  :init (setq evil-want-keybinding nil)
  :config
  (evil-mode t)
  (setq evil-move-cursor-back nil
        evil-move-beyond-eol t

        evil-undo-system 'undo-fu
        evil-want-fine-undo t

        evil-mode-line-format 'before
        evil-normal-state-cursor '(box "black")
        evil-insert-state-cursor '(box "green")
        evil-visual-state-cursor '(box "#F86155")))

(use-package evil-collection
  :straight t
  :after (evil)
  :config (evil-collection-init))

(use-package evil-surround
  :straight t
  :config (global-evil-surround-mode 1))

;; Enable copy-paste to/from x clipboard when running in a terminal
(use-package xclip
  :straight t
  :config (xclip-mode t))

(use-package seq)
(use-package cl-lib)

(use-package which-key
  :straight t
  :after (evil-leader seq cl-lib)
  :config
  (which-key-mode t))

(use-package winum
  :straight t
  :config (winum-mode t))

;;; Lisp setup

(use-package clojure-mode
  :straight t)

(use-package cider
  :straight t
  :after (clojure-mode))

(use-package smartparens
  :straight t
  :init
  (require 'smartparens-config)
  (add-hook 'cider-clojure-interaction-mode-hook 'smartparens-mode)
  (add-hook 'cider-repl-mode-hook 'smartparens-mode)
  (add-hook 'clojurex-mode-hook 'smartparens-mode)
  (add-hook 'clojurescript-mode-hook 'smartparens-mode)
  (add-hook 'clojurec-mode-hook 'smartparens-mode)
  (add-hook 'clojure-mode-hook 'smartparens-mode)
  (add-hook 'emacs-lisp-mode-hook 'smartparens-mode))

(use-package evil-cleverparens
  :straight t
  ;; disabling these initial bindings because it changes M-d (kill-word) to
  ;; evil-cp-delete-sexp, and I happen to use M-d a lot. Might want to review
  ;; cause some of these seem useful.
  :init (setq evil-cleverparens-use-additional-bindings nil)
  :after (evil clojure-mode smartparens)
  :hook (smartparens-mode . evil-cleverparens-mode))

(use-package aggressive-indent
  :straight t
  :hook (smartparens-mode . aggressive-indent-mode))

(use-package company
  :straight t
  :bind ("TAB" . company-complete)
  :init
  (add-hook 'cider-clojure-interaction-mode-hook 'company-mode)
  (add-hook 'cider-repl-mode-hook 'company-mode)
  (add-hook 'clojurex-mode-hook 'company-mode)
  (add-hook 'clojurescript-mode-hook 'company-mode)
  (add-hook 'clojurec-mode-hook 'company-mode)
  (add-hook 'clojure-mode-hook 'company-mode)
  (add-hook 'emacs-lisp-mode-hook 'company-mode))

(use-package projectile :straight t
  :config (projectile-global-mode))

(use-package counsel-projectile
  :straight t
  :after (projectile))

(use-package elisp-slime-nav :straight t
  :config
  (add-hook 'emacs-lisp-mode-hook 'turn-on-elisp-slime-nav-mode)
  (add-hook 'ielm-mode-hook 'turn-on-elisp-slime-nav-mode))

(use-package evil-multi-leader
  :after (evil which-key)
  :config
  (global-multi-leader-mode 1))

(use-package pprint-to-buffer
  :straight (pprint-to-buffer
             :type git
             :host github
             :files ("pprint-to-buffer/pprint-to-buffer.el")
             :repo "plexus/plexmacs"))

(use-package walkclj
  :straight (walkclj
             :type git
             :host github
             :repo "plexus/walkclj"))

;; Use the Clojure ns name as buffer name
(use-package clj-ns-name
  :after (walkclj)
  :straight (clj-ns-name
             :type git
             :host github
             :files ("clj-ns-name/clj-ns-name.el")
             :repo "plexus/plexmacs")
  :config
  (clj-ns-name-install)
  ;; Apply ns-name-as-buffer-name when jumping to definition
  (advice-add #'cider-find-file
              :around
              (lambda (cider-find-file-fn url)
                (let ((result (funcall cider-find-file-fn url)))
                  (clj-ns-name-rename-clj-buffer-to-namespace*)
                  result))))
;; (clj-ns-name-uninstall)

(desktop-save-mode)
(server-start)
(set-frame-font "Iosevka Fixed SS14-14")
(global-linum-mode 1)

(add-hook 'before-save-hook
          (lambda ()
            (when (derived-mode-p 'prog-mode)
              (delete-trailing-whitespace))))
