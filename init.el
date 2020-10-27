(add-to-list #'load-path (expand-file-name "init.d" user-emacs-directory))

(require 'straight-init)
(require 'better-defaults)
(require 'lesser-evil-commands)
(require 'better-emacs-lisp)

(setq straight-profiles '((lesser-evil . "lesser-evil.el")
                          (lesser-evil-user . "lesser-evil-user.el")))

(setq straight-current-profile 'lesser-evil)

(straight-use-package 'use-package)
(setq straight-use-package-by-default t)

(use-package ivy
  :defer 0.1
  :diminish
  :config
  (ivy-mode)
  (define-key ivy-minibuffer-map (kbd "C-j") #'ivy-next-line)
  (define-key ivy-minibuffer-map (kbd "C-k") #'ivy-previous-line))

(use-package counsel
  :after (ivy))

(use-package swiper
  :after (ivy))

(use-package undo-fu)

(use-package evil
  :init (setq evil-want-keybinding nil)
  :config
  (evil-mode t)
  (setq evil-move-cursor-back nil
        evil-move-beyond-eol t

        evil-undo-system 'undo-fu
        evil-want-fine-undo t

        evil-mode-line-format 'before
        evil-normal-state-cursor '(box "orange")
        evil-insert-state-cursor '(box "green")
        evil-visual-state-cursor '(box "#F86155"))

  (define-key evil-motion-state-map (kbd "TAB") #'indent-for-tab-command))

(use-package evil-collection
  :after (evil)
  :config (evil-collection-init))

(use-package evil-surround
  :config (global-evil-surround-mode 1))

;; Enable copy-paste to/from x clipboard when running in a terminal
(use-package xclip
  :config (xclip-mode t))

(use-package seq)
(use-package cl-lib)

(use-package which-key
  :after (evil-leader seq cl-lib)
  :config
  (which-key-mode t))

(use-package winum
  :config (winum-mode t))

;;; Lisp setup

(use-package clojure-mode
  :config
  (setq clojure-toplevel-inside-comment-form t))

(use-package cider
  :after (clojure-mode)
  :config
  (setq nrepl-log-messages t) ;; make sure we can always debug nrepl issues
  (set-register ?k "(do (require 'kaocha.repl) (kaocha.repl/run))")
  (set-register ?K "(do (require 'kaocha.repl) (kaocha.repl/run-all))")
  (set-register ?r "(user/refresh)")
  (set-register ?g "(user/go)"))

(use-package clj-refactor
  :after (cider)
  :config
  (setq cljr-cljc-clojure-test-declaration "[clojure.test :refer [deftest testing is are use-fixtures run-tests join-fixtures]]"
        cljr-cljs-clojure-test-declaration "[clojure.test :refer [deftest testing is are use-fixtures run-tests join-fixtures]]"
        cljr-clojure-test-declaration "[clojure.test :refer [deftest testing is are use-fixtures run-tests join-fixtures]]"
        cljr-eagerly-build-asts-on-startup nil
        cljr-warn-on-eval nil)
  (add-hook 'cider-clojure-interaction-mode-hook 'clj-refactor-mode)
  (add-hook 'clojurex-mode-hook 'clj-refactor-mode)
  (add-hook 'clojurescript-mode-hook 'clj-refactor-mode)
  (add-hook 'clojurec-mode-hook 'clj-refactor-mode)
  (add-hook 'clojure-mode-hook 'clj-refactor-mode))

(use-package smartparens
  :init
  (require 'smartparens-config)
  (add-hook 'cider-clojure-interaction-mode-hook 'smartparens-mode)
  (add-hook 'cider-repl-mode-hook 'smartparens-mode)
  (add-hook 'clojurex-mode-hook 'smartparens-mode)
  (add-hook 'clojurescript-mode-hook 'smartparens-mode)
  (add-hook 'clojurec-mode-hook 'smartparens-mode)
  (add-hook 'clojure-mode-hook 'smartparens-mode)
  (add-hook 'emacs-lisp-mode-hook 'smartparens-mode)
  (add-hook 'lisp-data-mode-hook 'smartparens-mode))

(use-package evil-cleverparens
  ;; disabling these initial bindings because it changes M-d (kill-word) to
  ;; evil-cp-delete-sexp, and I happen to use M-d a lot. Might want to review
  ;; cause some of these seem useful.
  :init (setq evil-cleverparens-use-additional-bindings nil)
  :config
  ;; evil-cp-regular-bindings contain various overrides of fundamental vim/evil
  ;; commands like x Y P C D etc. This seems a little too clever IMO. e.g. x
  ;; deletes a char or splices when on a delimiter, well we have ds( for that.
  ;; In particular I need to be able to delete a single parenthesis sometimes to
  ;; fix up a mess, and this really gets in the way. I may undefine more of
  ;; these in the future.
  (setq evil-cp-regular-bindings (delq (assoc "x" evil-cp-regular-bindings) evil-cp-regular-bindings))
  :after (evil clojure-mode smartparens)
  :hook (smartparens-mode . evil-cleverparens-mode))

(use-package aggressive-indent
  :init
  (add-hook 'cider-clojure-interaction-mode-hook 'aggressive-indent-mode)
  (add-hook 'clojurex-mode-hook 'aggressive-indent-mode)
  (add-hook 'clojurescript-mode-hook 'aggressive-indent-mode)
  (add-hook 'clojurec-mode-hook 'aggressive-indent-mode)
  (add-hook 'clojure-mode-hook 'aggressive-indent-mode)
  (add-hook 'emacs-lisp-mode-hook 'aggressive-indent-mode)
  (add-hook 'lisp-data-mode-hook 'aggressive-indent-mode))

(use-package company
  :init
  (add-hook 'cider-clojure-interaction-mode-hook 'company-mode)
  (add-hook 'cider-repl-mode-hook 'company-mode)
  (add-hook 'clojurex-mode-hook 'company-mode)
  (add-hook 'clojurescript-mode-hook 'company-mode)
  (add-hook 'clojurec-mode-hook 'company-mode)
  (add-hook 'clojure-mode-hook 'company-mode)
  (add-hook 'emacs-lisp-mode-hook 'company-mode))

(use-package projectile
  :config (projectile-global-mode))

(use-package counsel-projectile
  :after (projectile))

(use-package elisp-slime-nav
  :config
  (add-hook 'emacs-lisp-mode-hook 'turn-on-elisp-slime-nav-mode)
  (add-hook 'ielm-mode-hook 'turn-on-elisp-slime-nav-mode))

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
;; Occasionally clj-ns-name will error when trying to parse an ns form. This
;; needs fixing, but meanwhile you can disable it manually if necessary (this is
;; fairly rare)
;; (clj-ns-name-uninstall)

;; Not hooked up yet, but a good fallback for cases where smarter jumping is not
;; available
(use-package dumb-jump)

(use-package goto-last-change)

(use-package magit)

(use-package evil-magit
  :after (magit))

(require 'evil-multi-leader)
(global-multi-leader-mode 1)

;; Patches

;; Modified version, if no REPL of the right type is found then switch to any
;; REPL. This also makes sure that ,sq will kill any REPL, not just one that
;; corresponds with the current major mode (clj vs cljs)
(defun cider-switch-to-repl-buffer (&optional set-namespace)
  (interactive "P")
  (cider--switch-to-repl-buffer
   (or (cider-current-repl nil) (cider-current-repl 'any 'ensure))
   set-namespace))

;; User config

(setq straight-current-profile 'lesser-evil-user)

(let ((user-config (expand-file-name "lesser-evil-user-config.el" user-emacs-directory)))
  (when (file-exists-p user-config)
    (load user-config)))
