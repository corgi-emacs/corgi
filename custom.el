(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-enabled-themes '(leuven))
 '(safe-local-variable-values
   '((eval progn
           (setenv "MELVN__SERVER_PORT"
                   (or
                    (getenv "MELVN__SERVER_PORT")
                    "8078"))
           (setenv "MELVN__DATOMIC_URI"
                   (or
                    (getenv "MELVN__DATOMIC_URI")
                    "datomic:dev://localhost:4334/onze"))
           (setenv "MB_DB_FILE"
                   (or
                    (getenv "MB_DB_FILE")
                    "../metabase/metabase.db;AUTO_SERVER=TRUE")))
     (cider-refresh-after-fn . "reloaded.repl/resume")
     (cider-refresh-before-fn . "reloaded.repl/suspend")
     (eval
      (lambda nil
        (let
            ((init-file-path
              (expand-file-name "emacs.d/nextjournal.el" default-directory)))
          (when
              (file-exists-p init-file-path)
            (load init-file-path)
            (require 'nextjournal)))))
     (cider-refresh-after-fn . "com.nextjournal.journal.repl/post-refresh")
     (cider-refresh-before-fn . "com.nextjournal.journal.repl/pre-refresh")
     (elisp-lint-indent-specs
      (if-let* . 2)
      (when-let* . 1)
      (let* . defun)
      (nrepl-dbind-response . 2)
      (cider-save-marker . 1)
      (cider-propertize-region . 1)
      (cider-map-repls . 1)
      (cider--jack-in . 1)
      (cider--make-result-overlay . 1)
      (insert-label . defun)
      (insert-align-label . defun)
      (insert-rect . defun)
      (cl-defun . 2)
      (with-parsed-tramp-file-name . 2)
      (thread-first . 1)
      (thread-last . 1))
     (checkdoc-package-keywords-flag)
     (eval define-clojure-indent
           (assoc 0)
           (ex-info 0)
           (for! 1)
           (for* 1)
           (as-> 2)
           (uix/context-provider 1)
           (ductile\.ui\.commands\.api/register! 1)
           (ductile\.ui\.commands\.api/register-context-fn! 1)
           (commands/register! 1))
     (eval progn
           (make-variable-buffer-local 'cider-jack-in-nrepl-middlewares)
           (add-to-list 'cider-jack-in-nrepl-middlewares "shadow.cljs.devtools.server.nrepl/middleware"))
     (web-mode-markup-indent-offset . default-indent)
     (web-mode-css-indent-offset . default-indent)
     (web-mode-code-indent-offset . default-indent)
     (javascript-indent-level . default-indent)
     (css-indent-offset . default-indent)
     (default-indent . 2)
     (js2-mode-show-strict-warnings)
     (magit-save-repository-buffers . dontask)
     (frame-resize-pixelwise . t)
     (display-line-numbers-width-start . t)
     (cljr-magic-requires)
     (cider-save-file-on-load)
     (cider-repl-display-help-banner)
     (cider-auto-track-ns-form-changes))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
