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

;; mode-line-format
;; minor-mode-alist
(use-package diminish
  :diminish
  elisp-slime-nav-mode
  eldoc-mode
  subword-mode)

(use-package ivy
  :defer 0.1
  :diminish
  :config
  (ivy-mode)
  (define-key ivy-minibuffer-map (kbd "C-j") #'ivy-next-line)
  (define-key ivy-minibuffer-map (kbd "C-k") #'ivy-previous-line))

(use-package counsel
  :after (ivy)
  :config
  ;; This ensures that SPC f r (counsel-recentf, show recently opened files)
  ;; actually works
  (recentf-mode 1))

;; Make counsel-M-x show most recently used commands first
(use-package smex)

(use-package swiper
  :after (ivy))

(use-package avy)

(use-package undo-fu)

(use-package evil
  :init (setq evil-want-keybinding nil)
  :config
  (evil-mode t)
  (evil-set-undo-system 'undo-fu)
  (setq evil-move-cursor-back nil
        evil-move-beyond-eol t
        evil-want-fine-undo t
        evil-mode-line-format 'before
        evil-normal-state-cursor '(box "orange")
        evil-insert-state-cursor '(box "green")
        evil-visual-state-cursor '(box "#F86155")))

(use-package evil-collection
  :after (evil)
  :config
  (evil-collection-init)
  ;; Stop changing how last-sexp works. We don't move cursor back and allow
  ;; beyond-eol, so it makes no sense to munge this.
  (cl-loop
   for fun in '(elisp--preceding-sexp cider-last-sexp pp-last-sexp)
   do
   (advice-mapc
    (lambda (advice _props) (advice-remove fun advice))
    fun)))

(use-package evil-surround
  :config (global-evil-surround-mode 1))

;; Enable copy-paste to/from x clipboard when running in a terminal
(use-package xclip
  :config (xclip-mode t))

(use-package seq)
(use-package cl-lib)

(use-package which-key
  :after (evil-leader seq cl-lib)
  :diminish which-key-mode
  :config
  (which-key-mode t))

(use-package winum
  :config (winum-mode t))

;;; Lisp setup

magic-mode-alist

(use-package clojure-mode
  :magic ("^#![^\n]*/\\(clj\\|clojure\\|bb\\|lumo\\)" . clojure-mode)
  :config
  (setq clojure-toplevel-inside-comment-form t
        ;; Because of CIDER's insistence to send forms to all linked REPLs, we
        ;; *have* to be able to switch cljc buffer to clj/cljs mode without
        ;; cider complaining.
        clojure-verify-major-mode nil)

  (defun lesser-evil/clojure-backward-sexp-advice (command &rest args)
    (if (and (called-interactively-p 'any) (derived-mode-p 'clojure-mode))
        (funcall #'clojure-backward-logical-sexp (when (numberp (car args)) (car args)))
      (apply command args)))

  (defun lesser-evil/clojure-forward-sexp-advice (command &rest args)
    (if (and (called-interactively-p 'any) (derived-mode-p 'clojure-mode))
        (funcall #'clojure-forward-logical-sexp (when (numberp (car args)) (car args)))
      (apply command args)))

  (advice-add 'forward-sexp :around #'lesser-evil/clojure-forward-sexp-advice)
  (advice-add 'backward-sexp :around #'lesser-evil/clojure-backward-sexp-advice)
  (advice-add 'sp-forward-sexp :around #'lesser-evil/clojure-forward-sexp-advice)
  (advice-add 'sp-backward-sexp :around #'lesser-evil/clojure-backward-sexp-advice)
  (advice-add 'evil-cp-forward-sexp :around #'lesser-evil/clojure-forward-sexp-advice)
  (advice-add 'evil-cp-backward-sexp :around #'lesser-evil/clojure-backward-sexp-advice)
  )

(use-package cider
  :after (clojure-mode)
  :diminish cider-mode
  :config
  ;; make sure we can always debug nrepl issues
  (setq nrepl-log-messages t)

  ;; New function, should go upstream. Kill all associated REPLs
  (defun lesser-evil/cider-quit-all ()
    "Quit all current CIDER REPLs."
    (interactive)
    (let ((repls (seq-remove (lambda (r)
                               (equal r (get-buffer "*babashka-repl*")))
                             (seq-mapcat #'cdr (sesman-current-sessions 'CIDER)))))
      (seq-do #'cider--close-connection repls))
    ;; if there are no more sessions we can kill all ancillary buffers
    (cider-close-ancillary-buffers)
    ;; need this to refresh sesman browser
    (run-hooks 'sesman-post-command-hook))

  ;; When asking for a "matching" REPL (clj/cljs), and no matching REPL is found,
  ;; return any REPL that is there. This is so that cider-quit can be called
  ;; repeatedly to close all REPLs in a process. It also means that , s s will go
  ;; to any REPL if there is one open.
  (defun lesser-evil/around-cider-current-repl (command &optional type ensure)
    (or
     (if (not type)
         (or (funcall command nil)
             (funcall command 'any))
       (funcall command type))
     (get-buffer "*babashka-repl*")))

  (advice-add 'cider-current-repl :around #'lesser-evil/around-cider-current-repl)

  ;; This essentiall redefined cider-repls. The main thing it does is return all
  ;; REPLs by using sesman-current-sessions (plural) instead of
  ;; sesman-current-session. It also falls back to the babashka repl if no repls
  ;; are connected/linked, so we can always eval.
  (defun lesser-evil/around-cider-repls (command &optional type ensure)
    (let ((type (cond
                 ((listp type)
                  (mapcar #'cider-maybe-intern type))
                 ((cider-maybe-intern type))))
          (repls (delete-dups (seq-mapcat #'cdr (or (sesman-current-sessions 'CIDER)
                                                    (when ensure
                                                      (user-error "No linked %s sessions" system)))))))
      (or (seq-filter (lambda (b)
                        (and (cider--match-repl-type type b)
                             (not (equal b (get-buffer "*babashka-repl*")))))
                      repls)
          (list (get-buffer "*babashka-repl*")))))

  (advice-add 'cider-repls :around #'lesser-evil/around-cider-repls))

(use-package clj-refactor
  :after (cider)
  :diminish clj-refactor-mode
  :config
  (setq cljr-cljc-clojure-test-declaration "[clojure.test :refer [deftest testing is are use-fixtures run-tests join-fixtures]]"
        cljr-cljs-clojure-test-declaration "[clojure.test :refer [deftest testing is are use-fixtures run-tests join-fixtures]]"
        cljr-clojure-test-declaration "[clojure.test :refer [deftest testing is are use-fixtures run-tests join-fixtures]]"
        cljr-eagerly-build-asts-on-startup nil
        cljr-warn-on-eval nil)
  ;; (add-hook 'cider-clojure-interaction-mode-hook 'clj-refactor-mode) ;; => is this actually a thing?
  (add-hook 'clojurex-mode-hook 'clj-refactor-mode)
  (add-hook 'clojurescript-mode-hook 'clj-refactor-mode)
  (add-hook 'clojurec-mode-hook 'clj-refactor-mode)
  (add-hook 'clojure-mode-hook 'clj-refactor-mode))

(use-package smartparens
  :init (require 'smartparens-config)
  :diminish smartparens-mode
  :hook (prog-mode . smartparens-mode))

(use-package a)

(use-package evil-cleverparens
  ;; disabling these initial bindings because it changes M-d (kill-word) to
  ;; evil-cp-delete-sexp, and I happen to use M-d a lot. Might want to review
  ;; cause some of these seem useful.
  :after (a)
  :diminish
  :init (setq evil-cleverparens-use-additional-bindings nil)
  :config
  ;; Restore some "normal" evil bindings. We have bindings and text objects for
  ;; structural editing, so no need to override (and make unavailable) these
  ;; "plain" operations.
  (setq evil-cp-regular-bindings
        (a-assoc evil-cp-regular-bindings
                 "x" 'evil-delete-char
                 "d" 'evil-delete
                 "D" 'evil-delete-line
                 "P" 'evil-paste-before
                 "y" 'evil-yank
                 "Y" 'evil-yank-line))
  (evil-cp--enable-regular-bindings)
  :after (evil clojure-mode smartparens)
  :hook (smartparens-mode . evil-cleverparens-mode))

(use-package aggressive-indent
  :diminish aggressive-indent-mode
  :init
  (add-hook 'cider-clojure-interaction-mode-hook 'aggressive-indent-mode)
  (add-hook 'clojurex-mode-hook 'aggressive-indent-mode)
  (add-hook 'clojurescript-mode-hook 'aggressive-indent-mode)
  (add-hook 'clojurec-mode-hook 'aggressive-indent-mode)
  (add-hook 'clojure-mode-hook 'aggressive-indent-mode)
  (add-hook 'emacs-lisp-mode-hook 'aggressive-indent-mode)
  (add-hook 'lisp-data-mode-hook 'aggressive-indent-mode))

(use-package company
  :diminish company-mode
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

(use-package org)

(use-package markdown-mode)

(require 'evil-multi-leader)
(global-multi-leader-mode 1)

;; Move to the front so these keys always have priority
(setq minor-mode-map-alist (cons
                            (cons 'multi-leader-mode eml/map)
                            (delq multi-leader-mode minor-mode-map-alist)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Patches


(defadvice cider-find-var (before add-evil-jump activate)
  (evil-set-jump))

;; Modified to account for the string that babashka outputs


;; Offer to create parent directories if they do not exist
;; http://iqbalansari.github.io/blog/2014/12/07/automatically-create-parent-directories-on-visiting-a-new-file-in-emacs/
(defun magnars/create-non-existent-directory ()
  (let ((parent-directory (file-name-directory buffer-file-name)))
    (when (and (not (file-exists-p parent-directory))
               (y-or-n-p (format "Directory `%s' does not exist! Create it?" parent-directory)))
      (make-directory parent-directory t))))

(add-to-list 'find-file-not-found-functions 'magnars/create-non-existent-directory)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; User config

(setq straight-current-profile 'lesser-evil-user)

(let ((user-config (expand-file-name "lesser-evil-user-config.el" user-emacs-directory)))
  (when (file-exists-p user-config)
    (load user-config)))
