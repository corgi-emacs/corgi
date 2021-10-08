(load-file (expand-file-name "bootstrap.el" user-emacs-directory))

(use-package magit)
(use-package org
  :config
  (require 'org-tempo))
(use-package markdown-mode)
(use-package yaml-mode)

(straight-freeze-versions)
(server-start)

;; use with ,,<letter>, e.g. `,,g' runs (user/go), see corgi/cider-pprint-register
(set-register ?g "#_clj (user/go)")

(use-package color-theme-sanityinc-tomorrow
  :config
  (load-theme 'sanityinc-tomorrow-bright t))
