;; Bring some of the nice things from CIDER/clojure-mode to Emacs lisp

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Show emacs-lisp eval results in an overlay, CIDER style. Depends on CIDER

;; https://endlessparentheses.com/eval-result-overlays-in-emacs-lisp.html

(defun lesser-evil/eval-overlay (value point)
  (cider--make-result-overlay (format "%S" value)
    :where point
    :duration 'command)
  ;; Preserve the return value.
  value)

(advice-add 'eval-region :around
            (lambda (f beg end &rest r)
              (lesser-evil/eval-overlay
               (apply f beg end r)
               end)))

(advice-add 'eval-last-sexp :filter-return
            (lambda (r)
              (lesser-evil/eval-overlay r (point))))

(advice-add 'eval-defun :filter-return
            (lambda (r)
              (lesser-evil/eval-overlay
               r
               (save-excursion
                 (end-of-defun)
                 (point)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Thread-first / thread-last, ported from clojure-mode

(defun lesser-evil/thread ()
  "Thread by one more level an existing threading macro."
  (interactive)
  (ignore-errors
    (when (looking-at "(")
      (forward-char 1)
      (forward-sexp 1)))
  (search-backward-regexp "(thread-")
  (down-list)
  (when (clojure--threadable-p)
    (prog1 (cond
            ((looking-at "thread-first")  (clojure--thread-first))
            ((looking-at "thread-last") (clojure--thread-last)))
      (clojure--fix-sexp-whitespace 'move-out))))

(defun lesser-evil/elisp--thread-all (first-or-last-thread but-last)
  (save-excursion
    (insert-parentheses 1)
    (insert first-or-last-thread))
  (while (save-excursion (lesser-evil/thread)))
  (when (or but-last clojure-thread-all-but-last)
    (clojure-unwind)))

(defun lesser-evil/elisp-thread-first-all (but-last)
  (interactive "P")
  (lesser-evil/elsip--thread-all "thread-first " but-last))

(defun lesser-evil/elisp-thread-last-all (but-last)
  (interactive "P")
  (lesser-evil/elisp--thread-all "thread-last " but-last))


(provide 'better-emacs-lisp)
