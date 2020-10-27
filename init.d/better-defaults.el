;; * BETTER DEFAULTS *

;; Various things that really should have been configured this way
;; out of the box. This is mostly copied from Magnar Sveen's config,
;; but some opt-in behavior (e.g. winner-mode) has been moved to
;; setup-emacs.el, and configuration related to packages (e.g.
;; smooth-scrolling), is in setup-packages.el

;; Turn off mouse interface early in startup to avoid momentary display
(if (fboundp 'menu-bar-mode) (menu-bar-mode -1))
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))

(setq inhibit-startup-message t)

;; Allow pasting selection outside of Emacs
(setq select-enable-clipboard t)

;; Auto refresh buffers
(global-auto-revert-mode 1)
;; (diminish 'auto-revert-mode)

;; Also auto refresh dired, but be quiet about it
(setq global-auto-revert-non-file-buffers t)
(setq auto-revert-verbose nil)

;; Show keystrokes in progress
(setq echo-keystrokes 0.1)

;; Move files to trash when deleting
;; (setq delete-by-moving-to-trash t)

;; Real emacs knights don't use shift to mark things
(setq shift-select-mode nil)

;; Transparently open compressed files
(auto-compression-mode t)

;; Enable syntax highlighting for older Emacsen that have it off
(global-font-lock-mode t)

;; Answering just 'y' or 'n' will do
(defalias 'yes-or-no-p 'y-or-n-p)

;; UTF-8 please
(setq locale-coding-system 'utf-8) ; pretty
(set-terminal-coding-system 'utf-8) ; pretty
(set-keyboard-coding-system 'utf-8) ; pretty
(set-selection-coding-system 'utf-8) ; please
(prefer-coding-system 'utf-8) ; with sugar on top

;; Show active region
(transient-mark-mode 1)
(make-variable-buffer-local 'transient-mark-mode)
(put 'transient-mark-mode 'permanent-local t)
(setq-default transient-mark-mode t)

;; Don't highlight matches with jump-char - it's distracting
;; (setq jump-char-lazy-highlight-face nil)

;; Always display line and column numbers
(setq line-number-mode t)
(setq column-number-mode t)

;; Lines should be 80 characters wide, not 72
(setq fill-column 80)
(set-default 'fill-column 80)

;; Never insert tabs
(set-default 'indent-tabs-mode nil)

;; Show me empty lines after buffer end
(set-default 'indicate-empty-lines t)

;; Easily navigate sillycased words
(global-subword-mode 1)
;; (diminish 'subword-mode)

;; Don't break lines for me, please
;; (setq-default truncate-lines t)

;; Allow recursive minibuffers
(setq enable-recursive-minibuffers t)

;; Don't be so stingy on the memory, we have lots now. It's the distant future.
;;(setq gc-cons-threshold 2000000)

;; Sentences do not need double spaces to end. Period.
(set-default 'sentence-end-double-space nil)

;; Add parts of each file's directory to the buffer name if not unique
(require 'uniquify)
(setq uniquify-buffer-name-style 'forward)

;; A betterr ediff
;; (setq ediff-diff-options "-w")
;; (setq ediff-split-window-function 'split-window-horizontally)
;; (setq ediff-window-setup-function 'ediff-setup-windows-plain)

;; No electric indent
(setq electric-indent-mode nil)

;; Nic says eval-expression-print-level needs to be set to nil (turned off) so
;; that you can always see what's happening.
(setq eval-expression-print-level nil)

;; When popping the mark, continue popping until the cursor actually moves
;; Also, if the last command was a copy - skip past all the expand-region cruft.
;; (defadvice pop-to-mark-command (around ensure-new-position activate)
;;   (let ((p (point)))
;;     (when (eq last-command 'save-region-or-current-line)
;;       ad-do-it
;;       ad-do-it
;;       ad-do-it)
;;     (dotimes (i 10)
;;       (when (= p (point)) ad-do-it))))

;; Offer to create parent directories if they do not exist
;; http://iqbalansari.github.io/blog/2014/12/07/automatically-create-parent-directories-on-visiting-a-new-file-in-emacs/
(defun magnars/create-non-existent-directory ()
  (let ((parent-directory (file-name-directory buffer-file-name)))
    (when (and (not (file-exists-p parent-directory))
               (y-or-n-p (format "Directory `%s' does not exist! Create it?" parent-directory)))
      (make-directory parent-directory t))))

(add-to-list 'find-file-not-found-functions 'magnars/create-non-existent-directory)

;; disable fringes... not sure yet if I want this
;; (fringe-mode 0)
;;(fringe-mode nil) ;; reset to default

;; Keep org-mode timestamps in English, e.g. [2016-11-05 Sat 10:03]
;; (setq system-time-locale "C")

;; Improves rendering of long lines.
;; Not for everyone. If you plan on working with Arabic or other bidi languages don't use this.
;; (setq-default bidi-display-reordering nil)

;; Don't make backup~ files
(setq make-backup-files nil)

;; Stop asking about following symlinks to version controlled files
(setq vc-follow-symlinks t)

(provide 'better-defaults)
