;;; -*- no-byte-compile: t -*-

;; This is your user signals file, here you configure how certain signals are
;; handled in specific modes.

((emacs-lisp-mode ( :eval/last-sexp eval-last-sexp
                    :eval/buffer eval-buffer
                    :eval/last-sexp-pprint pprint-to-buffer-last-sexp
                    :eval/region eval-region
                    :eval/outer-sexp eval-defun
                    :repl/toggle ielm
                    :eval/last-sexp-pprint-comment pprint-to-buffer-last-sexp-to-current-buffer

                    :refactor/thread-first corgi/elisp-thread-first-all
                    :refactor/thread-last corgi/elisp-thread-last-all)))
