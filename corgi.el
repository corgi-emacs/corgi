;;; corgi.el --- Emacs config based on Straight, Evil, and CIDER
;;
;; Filename: corgi.el
;; Description:
;; Author: Arne Brasseur <arne@lambdaisland.com>
;; Version: 0.1.0
;; Package-Requires:
;; URL: https://github.com/lambdaisland/corgi
;; Doc URL:
;; Keywords:
;; Compatibility:
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Commentary:
;;
;;
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Change Log:
;;
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or (at
;; your option) any later version.
;;
;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <https://www.gnu.org/licenses/>.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Code:


(require 'corgi-commands)
(require 'corgi-better-emacs-lisp)

(use-package a)

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
        evil-visual-state-cursor '(box "#F86155")
        evil-emacs-state-cursor  '(box "purple"))

  ;; Prevent evil-motion-state from shadowing previous/next sexp
  (require 'evil-maps)
  (define-key evil-motion-state-map "L" nil)
  (define-key evil-motion-state-map "M" nil))

(use-package evil-collection
  :after (evil)
  :config
  (evil-collection-init)
  ;; Stop changing how last-sexp works. Even though we have evil-move-beyond-eol
  ;; set, this is still being added, and I can't figure out why. Resorting to
  ;; this hack.
  (cl-loop
   for fun
   in '(elisp--preceding-sexp cider-last-sexp pp-last-sexp)
   do (advice-mapc (lambda (advice _props) (advice-remove fun advice)) fun)))

(use-package evil-surround
  :config (global-evil-surround-mode 1))

(use-package which-key
  :diminish which-key-mode
  :config
  (which-key-mode 1))

(use-package winum
  :config (winum-mode 1))

(use-package corkey
  :config
  (global-corkey-mode 1)
  ;; Move to the front so these keys always have priority
  (setq minor-mode-map-alist
        (cons
         (cons 'corkey-mode corkey/keymap)
         (delq corkey-mode minor-mode-map-alist)))
  (add-to-list #'corkey/key-binding-files (expand-file-name "corgi-bindings.el" (file-name-directory (or load-file-name buffer-file-name)))))

;;; Lisp setup

(use-package clojure-mode
  :magic ("^#![^\n]*/\\(clj\\|clojure\\|bb\\|lumo\\)" . clojure-mode)
  :config
  (setq clojure-toplevel-inside-comment-form t
        ;; Because of CIDER's insistence to send forms to all linked REPLs, we
        ;; *have* to be able to switch cljc buffer to clj/cljs mode without
        ;; cider complaining.
        clojure-verify-major-mode nil)

  ;; TODO: get this upstream. #_ is not a logical sexp
  (defun corgi/clojure--looking-at-non-logical-sexp (command)
    "Return non-nil if text after point is \"non-logical\" sexp.
\"Non-logical\" sexp are ^metadata and #reader.macros."
    (comment-normalize-vars)
    (comment-forward (point-max))
    (looking-at-p "\\(?:#?\\^\\)\\|#:?:?[[:alpha:]]\\|#_"))

  (advice-add #'clojure--looking-at-non-logical-sexp :around #'corgi/clojure--looking-at-non-logical-sexp))

(use-package cider
  :after (clojure-mode)
  :diminish cider-mode
  :config
  (setq cider-preferred-build-tool 'clojure-cli
        ;; make sure we can always debug nrepl issues
        nrepl-log-messages t)

  ;; TODO: clean this up, submit to upstream where possible
  ;; More CIDER/clojure-mode stuff
  ;; - logical-sexp doesn't treat #_ correctly

  ;; New function, should go upstream. Kill all associated REPLs
  (defun corgi/cider-quit-all ()
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
  (defun corgi/around-cider-current-repl (command &optional type ensure)
    (let ((repl (or
                 (if (not type)
                     (or (funcall command nil)
                         (funcall command 'any))
                   (funcall command type))
                 (get-buffer "*babashka-repl*"))))
      (if (and ensure (null repl))
          (cider--no-repls-user-error type)
        repl)))

  (advice-add #'cider-current-repl :around #'corgi/around-cider-current-repl)

  ;; This essentially redefines cider-repls. The main thing it does is return all
  ;; REPLs by using sesman-current-sessions (plural) instead of
  ;; sesman-current-session. It also falls back to the babashka repl if no repls
  ;; are connected/linked, so we can always eval.
  (defun corgi/around-cider-repls (command &optional type ensure)
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

  (advice-add #'cider-repls :around #'corgi/around-cider-repls)

  (defun corgi/cider-eval-last-sexp-and-replace ()
    "Alternative to cider-eval-last-sexp-and-replace, but kills
clojure logical sexp instead of ELisp sexp, and pprints the
result."
    (interactive)
    (let ((last-sexp (cider-last-sexp)))
      ;; we have to be sure the evaluation won't result in an error
      (cider-nrepl-sync-request:eval last-sexp)
      ;; seems like the sexp is valid, so we can safely kill it
      (let ((opoint (point)))
        (clojure-backward-logical-sexp)
        (kill-region (point) opoint))
      (cider-interactive-eval last-sexp
                              (cider-eval-pprint-with-multiline-comment-handler
                               (current-buffer)
                               (set-marker (make-marker) (point))
                               ""
                               " "
                               "")
                              nil
                              (cider--nrepl-print-request-map fill-column))))

  (defun corgi/cider-pprint-eval-last-sexp-insert ()
    (interactive)
    (let ((cider-comment-prefix "")
          (cider-comment-continued-prefix " ")
          (cider-comment-postfix ""))
      (cider-pprint-eval-last-sexp-to-comment))))

(use-package clj-refactor
  :after (cider)
  :diminish clj-refactor-mode
  :config
  (setq cljr-cljc-clojure-test-declaration "[clojure.test :refer [deftest testing is are use-fixtures run-tests join-fixtures]]"
        cljr-cljs-clojure-test-declaration "[clojure.test :refer [deftest testing is are use-fixtures run-tests join-fixtures]]"
        cljr-clojure-test-declaration "[clojure.test :refer [deftest testing is are use-fixtures run-tests join-fixtures]]"
        cljr-eagerly-build-asts-on-startup nil
        cljr-warn-on-eval nil)
  :hook ((clojurex-mode-hook
          clojurescript-mode-hook
          clojurec-mode-hook
          clojure-mode-hook)
         . clj-refactor-mode))

(use-package smartparens
  :init (require 'smartparens-config)
  :diminish smartparens-mode
  :hook (prog-mode . smartparens-mode))

;; We don't actually enable cleverparens, because most of their bindings we
;; don't want, we install our own bindings for specific sexp movements
(use-package evil-cleverparens
  :after (evil clojure-mode smartparens))

(use-package aggressive-indent
  :diminish aggressive-indent-mode
  :hook ((clojurex-mode
          clojurescript-mode
          clojurec-mode
          clojure-mode
          emacs-lisp-mode
          lisp-data-mode)
         . aggressive-indent-mode))

(use-package rainbow-delimiters
  :hook ((cider-repl-mode
          clojurex-mode
          clojurescript-mode
          clojurec-mode
          clojure-mode
          emacs-lisp-mode
          lisp-data-mode
          inferior-emacs-lisp-mode)
         . rainbow-delimiters-mode))

(use-package company
  :diminish company-mode
  :hook (prog-mode . company-mode))

(use-package projectile
  :config
  (projectile-global-mode)
  (setq projectile-create-missing-test-files t))

(use-package counsel-projectile
  :after (projectile))

(use-package elisp-slime-nav
  :config
  :hook ((emacs-lisp-mode ielm-mode) . turn-on-elisp-slime-nav-mode))

(use-package pprint-to-buffer)

;; Use the Clojure ns name as buffer name
(use-package clj-ns-name
  :config
  (clj-ns-name-install)
  ;; Apply ns-name-as-buffer-name when jumping to defcorgiion
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

(use-package expand-region)

(use-package org
  :config
  ;; Make templates like <s work
  (require 'org-tempo))

(use-package markdown-mode)

(use-package yaml-mode)

(use-package string-edit)

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

(provide 'corgi)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; corgi.el ends here
