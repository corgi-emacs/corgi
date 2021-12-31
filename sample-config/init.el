(load-file (expand-file-name "bootstrap.el" user-emacs-directory))

;; Corgi is just a set of packages. You can use `M-x find-library' to check them
;; out. Or if you want to tweak things then just copy the code over to your
;; config and edit it there.

(let ((straight-current-profile 'corgi))
  ;; Change a bunch of Emacs defaults, from disabling the menubar and toolbar,
  ;; to fixing modifier keys on Mac and disabling the system bell.
  (use-package corgi-defaults)

  ;; UI configuration for that Corgi-feel. This sets up a bunch of packages like
  ;; Evil, Smartparens, Ivy (minibuffer completion), Swiper (fuzzy search),
  ;; Projectile (project-aware commands), Aggressive indent, Company
  ;; (completion).
  (use-package corgi-editor)

  ;; The few custom commands that we ship with. This includes a few things we
  ;; emulate from Spacemacs, and commands for jumping to the user's init.el
  ;; (this file, with `SPC f e i'), or opening the user's key binding or signals
  ;; file.
  (use-package corgi-commands)

  ;; Extensive setup for a good Clojure experience, including clojure-mode,
  ;; CIDER, and a modeline indicator that shows which REPLs your evaluations go
  ;; to.
  ;; Also contains `corgi/cider-pprint-eval-register', bound to `,,', see
  ;; `set-register' calls below.
  (use-package corgi-clojure
    :config
    (corgi/enable-cider-connection-indicator))

  ;; Emacs Lisp config, mainly to have a development experience that feels
  ;; similar to using CIDER and Clojure. (show results in overlay, threading
  ;; refactorings)
  (use-package corgi-emacs-lisp)

  ;; Change the color of the modeline based on the Evil state (e.g. green when
  ;; in insert state)
  (use-package corgi-stateline)

  ;; Corgi's keybinding system, which builds on top of Evil. See the manual, or
  ;; visit the key binding and signal files (with `SPC f e k', `SPC f e K', `SPC
  ;; f e s' `SPC f e S')
  (use-package corkey
    :config
    (corkey-mode 1)
    ;; Automatically pick up keybinding changes
    (corkey/load-and-watch)))

;; Load other useful packages you might like to use

(use-package magit)
(use-package org)
(use-package markdown-mode)
(use-package yaml-mode)
(use-package typescript-mode)
(use-package rainbow-mode)
(use-package treemacs)

;; Start the emacs-server, so you can open files from the command line with
;; `emacsclient -n <file>' (we like to put `alias en="emacsclient -n"' in our
;; shell config).
(server-start)

;; Emacs has "registers", places to keep small snippets of text. We make it easy
;; to run a snippet of Clojure code in such a register, just press comma twice
;; followed by the letter that designates the register (while in a Clojure
;; buffer with a connected REPL). The code will be evaluated, and the result
;; pretty-printed to a separate buffer.

;; By starting a snippet with `#_clj' or `#_cljs' you can control which type of
;; REPL it will go to, in case you have both a CLJ and a CLJS REPL connected.
(set-register ?k "#_clj (do (require 'kaocha.repl) (kaocha.repl/run))")
(set-register ?K "#_clj (do (require 'kaocha.repl) (kaocha.repl/run-all))")
(set-register ?r "#_clj (do (require 'user :reload) (user/reset))")
(set-register ?g "#_clj (user/go)")
(set-register ?b "#_clj (user/browse)")

;; We like this theme because it looks nice and works well enough on terminal,
;; swap it out with whatever suits you.
(use-package color-theme-sanityinc-tomorrow
  :config
  (load-theme 'sanityinc-tomorrow-bright t))

;; Not a fan of trailing whitespace in source files, strip it out when saving.
(add-hook 'before-save-hook
          (lambda ()
            (when (derived-mode-p 'prog-mode)
              (delete-trailing-whitespace))))

(let ((local-config (expand-file-name "local.el" user-emacs-directory)))
  (when (file-exists-p local-config)
    (load-file local-config)))
