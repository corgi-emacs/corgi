(add-to-list #'load-path (expand-file-name "init.d" user-emacs-directory))

(require 'better-defaults)
(require 'lesser-evil-commands)

(straight-use-package 'use-package)

(use-package ivy :straight t
  :defer 0.1
  :diminish
  :config (ivy-mode))

(use-package counsel :straight t
  :after ivy
  :config (counsel-mode))

(use-package swiper :straight t
  :after ivy)

(use-package evil :straight (evil :host github :repo "emacs-evil/evil")
  :init (setq evil-want-keybinding nil)
  :config
  (evil-mode t)
  (setq evil-move-cursor-back nil)
  (setq evil-move-beyond-eol t))

(use-package evil-leader :straight t
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
  :straight (evil-collection :type git
                             :host github
                             :repo "emacs-evil/evil-collection")
  :after (evil))

;; Enable copy-paste to/from x clipboard when running in a terminal
(use-package xclip :straight t
  :config (xclip-mode t))

(use-package seq)
(use-package cl-lib)

(defun install-leader-keys (bindings-file)
  "Load the key declarations from bindings-file, and use
them to set evil-leader keybindings, and which-key descriptions."
  (let ((bindings (with-temp-buffer
                    (insert-file-contents bindings-file)
                    (goto-char (point-min))
                    (read (current-buffer))))
        (flatten-bindings (lambda (prefix bindings)
                            (let ((p (car bindings))
                                  (desc (cadr bindings))
                                  (rest (cddr bindings)))
                              (if (symbolp (car rest))
                                  (list (concat prefix p) (car rest))
                                (seq-mapcat (lambda (b)
                                              (funcall flatten-bindings (concat prefix p) b))
                                            rest)))))
        (flatten-descriptions (lambda (prefix bindings)
                                (let ((p (car bindings))
                                      (desc (cadr bindings))
                                      (rest (cddr bindings)))
                                  (if (symbolp (car rest))
                                      (list (concat prefix p) desc)
                                    (cl-list* (concat prefix p) desc
                                              (seq-mapcat (lambda (b)
                                                            (funcall flatten-descriptions (concat prefix p) b))
                                                          rest)))))))
    (apply 'evil-leader/set-key (seq-mapcat (lambda (x) (funcall flatten-bindings "" x)) bindings))
    (apply 'which-key-add-key-based-replacements (seq-mapcat (lambda (x) (funcall flatten-descriptions "SPC " x)) bindings))))

(use-package which-key :straight t
  :after (evil-leader seq cl-lib)
  :config
  (which-key-mode t)
  (install-leader-keys (expand-file-name "leader_bindings.el" user-emacs-directory)))

(use-package winum :straight t
  :config (winum-mode t))

;;; Lisp setup

(use-package clojure-mode :straight t)

(use-package cider :straight t
  :after (clojure-mode))

(use-package smartparens :straight t
  :init
  (require 'smartparens-config)
  (add-hook 'cider-clojure-interaction-mode-hook 'smartparens-mode) 
  (add-hook 'cider-repl-mode-hook 'smartparens-mode)
  (add-hook 'clojurex-mode-hook 'smartparens-mode)
  (add-hook 'clojurescript-mode-hook 'smartparens-mode)
  (add-hook 'clojurec-mode-hook 'smartparens-mode)
  (add-hook 'clojure-mode-hook 'smartparens-mode)
  (add-hook 'emacs-lisp-mode-hook 'smartparens-mode))

(use-package evil-cleverparens :straight t
  :after (evil clojure-mode smartparens)
  :hook (smartparens-mode . evil-cleverparens-mode))

(use-package aggressive-indent :straight t
  :hook (smartparens-mode . aggressive-indent-mode))
