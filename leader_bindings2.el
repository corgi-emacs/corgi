(:bindings
 (("TAB" "Indent" indent-for-tab-command)

  ("SPC" "Global leader key"
   ("b" "Buffer commands"
    ("b" "Switch buffer" ivy-switch-buffer)
    ("d" "Kill buffer" kill-this-buffer))

   ("f" "File commands"
    ("f" "Find file" counsel-find-file)
    ("s" "Save file" save-buffer)
    ("S" "Save all" evil-write-all)
    ("A" "Find alternate file" find-alternate-file))

   ("s" "Search commands"
    ("s" "Search in buffer" swiper)
    ("p" "Grep in project" counsel-git-grep))

   ("p" "Project"
    ("f" "Find file" counsel-projectile-find-file)
    ("p" "Switch project" counsel-projectile-switch-project))

   ("h" "Help"
    ("d" "Describe"
     ("k" "Describe key" describe-key)
     ("v" "Describe variable" counsel-describe-variable)
     ("f" "Describe function" counsel-describe-function)
     ("m" "Describe mode" describe-mode)
     ("b" "Describe bindings" describe-bindings)))

   ("w" "Windows"
    ("TAB" "Alternate window" alternate-window)
    ("1" "Delete other windows" delete-other-windows)
    ("2" "Two column layout" lesser-evil/double-columns)
    ("/" "Split window right" split-window-right)
    ("-" "Split window below" split-window-below)
    ("o" "Go to other window" other-window)
    ("d" "Delete window" delete-window))

   ("k" "Structural editing"
    ("E" "Splice backwards" sp-splice-sexp-killing-backward))

   ("SPC" "Execute command (M-x)" counsel-M-x)
   ("TAB" "Switch to previous buffer" lesser-evil/switch-to-previous-buffer)
   ("1" "Select window 1" winum-select-window-1)
   ("2" "Select window 2" winum-select-window-2)
   ("3" "Select window 3" winum-select-window-3)
   ("4" "Select window 4" winum-select-window-4)
   ("5" "Select window 5" winum-select-window-5)
   ("6" "Select window 6" winum-select-window-6)
   ("7" "Select window 7" winum-select-window-7)
   ("8" "Select window 8" winum-select-window-8)
   ("9" "Select window 9" winum-select-window-9)
   ("0" "Select window 10" winum-select-window-10))

  ("," "Project specific leader key"

   ("e" "Evaluate expressions"
    ("b" "Eval buffer" :eval/buffer)
    ("e" "Eval form before cursor" :eval/last-sexp)
    ("p" "Eval and pretty print" :eval/last-sexp-pprint)
    ("P" "Eval to comment" :eval/last-sexp-pprint-comment)
    ("n" "Eval ns form" :eval/ns-form)
    ("r" "Eval region" :eval/region))

   ("s" "REPL"
    ("s" "Toggle REPL" :repl/toggle)
    ("q" "Quit REPL process" :repl/quit)
    ("o" "Switch to Other REPL" :repl/other))

   ("g" "Go places"
    ("g" "Jump to definition" :jump/definition)
    ("b" "Go back" :jump/back))

   ("l" "Link to REPL"
    ("p" "Link with project" sesman-link-with-project)
    ("b" "Link with buffer" sesman-link-with-buffer)
    ("d" "Link with directory" sesman-link-with-directory)
    ("l" "Link least specific" sesman-link-with-least-specific))

   ("'" "Jack in" :repl/jack-in)
   ("\"" "Jack in Alternate" :repl/jack-in-alt)
   ("&" "Jack in Combined" :repl/jack-in-combined)
   ("," "Eval from registry and pprint" :eval/registry-pprint)))

 ;; ","  'plexus-clojure-extras/cider-pprint-register

 :modes
 ((emacs-lisp-mode ( :eval/last-sexp eval-last-sexp
                     :eval/buffer eval-buffer
                     :eval/last-sexp-pprint pprint-to-buffer-last-sexp
                     :eval/region cider-eval-region
                     :repl/toggle ielm-mode

                     :jump/definition xref-find-definitions
                     :jump/back xref-pop-marker-stack))
  ;;  (ielm-mode ( :repl/toggle ))
  (clojure-mode ( :eval/last-sexp cider-eval-last-sexp
                  :eval/last-sexp-pprint cider-pprint-eval-last-sexp
                  :eval/last-sexp-pprint-comment cider-pprint-eval-last-sexp-to-comment
                  :eval/ns-form cider-eval-ns-form
                  :eval/last-sexp-replace cider-eval-last-sexp-and-replace
                  :eval/buffer cider-eval-buffer
                  :eval/region cider-eval-region
                  :eval/registry-pprint lesser-evil/cider-pprint-register

                  :repl/toggle lesser-evil/cider-toggle-repl
                  :repl/quit cider-quit
                  :repl/other cider-repl-switch-to-other

                  :jump/definition cider-find-var
                  :jump/back cider-pop-back

                  :repl/jack-in ("Jack-in Clojure" cider-jack-in-clj)
                  :repl/jack-in-alt ("Jack in ClojureScript" cider-jack-in-clj)
                  :repl/jack-in-combined ("Jack in Clj+Cljs" cider-jack-in-clj&cljs)))

  (c-mode ( :jump/definition xref-find-definitions
            :jump/back xref-pop-marker-stack))))
