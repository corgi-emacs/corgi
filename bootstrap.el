;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Straight

(defvar bootstrap-version)

(let ((install-url "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el")
      (bootstrap-file (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer (url-retrieve-synchronously install-url 'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

(straight-use-package 'use-package)

(setq straight-use-package-by-default t)

(when (not (file-exists-p (expand-file-name "straight/versions/default.el" straight-base-dir)))
  (straight-freeze-versions))

(use-package corgi-packages
  :straight (corgi-packages
             :type git
             :host github
             :repo "lambdaisland/corgi-packages"))

(add-to-list #'straight-recipe-repositories 'corgi-packages)

(let ((straight-current-profile 'corgi))
  (use-package corgi-defaults)
  (use-package corgi-editor)
  (use-package corgi-emacs-lisp)
  (use-package corgi-commands)
  (use-package corgi-clojure)
  (use-package corgi-stateline)
  (use-package corkey
    :config
    (corkey-mode 1)
    (corkey/install-bindings)))
