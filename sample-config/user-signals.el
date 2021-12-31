;;; -*- no-byte-compile: t -*-

;; This is your user signals file, here you configure how certain signals are
;; handled in specific modes.

((js-mode ( :eval/last-sexp js-send-last-sexp
            :eval/buffer js-send-buffer
            :eval/region js-send-region
            :repl/toggle js-comint-start-or-switch-to-repl
            )))
