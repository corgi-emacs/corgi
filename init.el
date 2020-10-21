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
  :after ivy
  ;;:config (counsel-mode)i
  )

(use-package swiper
  :straight t
  :after ivy)

(use-package evil
  :straight t
  :init (setq evil-want-keybinding nil)
  :config
  (evil-mode t)
  (setq evil-move-cursor-back nil)
  (setq evil-move-beyond-eol t))

(use-package evil-leader
  :straight t
  :after (evil)
  :commands (evil-leader-mode global-evil-leader-mode)
  :demand
  :config
  (evil-leader/set-leader "SPC")
  (global-evil-leader-mode t)
  (evil-leader/set-key
   "ff" 'counsel-find-file
   "bb" 'ivy-switch-buffer
   "ss" 'swiper))

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

(use-package projectile :straight t)

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

(desktop-save-mode)
(server-start)
