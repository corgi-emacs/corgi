;; Set up low-level stuff so we can install the various packages that make up
;; Corgi. Not super pretty, but you normally don't have to look at it.

;; Install the Straight package manager

(defvar bootstrap-version)

(let ((install-url "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el")
      (bootstrap-file (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer (url-retrieve-synchronously install-url 'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

;; Install the use-package convenience macro

(straight-use-package 'use-package)

(setq straight-use-package-by-default t)

(when (not (file-exists-p (expand-file-name "straight/versions/default.el" straight-base-dir)))
  (straight-freeze-versions))

;; Enable the corgi-packages repository so we can install our packages with
;; Straight. This also runs some Corgi initialization code, notably copying over
;; Corgi's version file, so you get the same versions of packages that Corgi was
;; tested with.

(use-package corgi-packages
  :straight (corgi-packages
             :type git
             :host github
             :repo "lambdaisland/corgi-packages"))

(add-to-list #'straight-recipe-repositories 'corgi-packages)
