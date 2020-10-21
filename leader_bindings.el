(("b" "Buffer commands"
  ("b" "Switch buffer" ivy-switch-buffer)
  ("d" "Kill buffer" kill-buffer))

 ("f" "File commands"
  ("f" "Find file" counsel-find-file)
  ("s" "Save file" save-buffer)
  ("S" "Save all" evil-write-all)
  ("A" "Find alternate file" find-alternate-file))

 ("s" "Search commands"
  ("s" "Search in buffer" swiper))

 ("h" "Help"
  ("d" "Describe"
   ("k" "Describe key" describe-key)
   ("v" "Describe variable" counsel-describe-variable)
   ("f" "Describe function" counsel-descbinds-function)
   ("m" "Describe mode" describe-mode)
   ("b" "Describe bindings" describe-bindings)))

 ("w" "Windows"
  ("1" "Delete other windows" delete-other-windows)
  ("/" "Split window right" split-window-right)
  ("-" "Split window below" split-window-below)
  ("o" "Go to other window" other-window)
  ("d" "Delete window" delete-window))

 ("SPC" "Execute command (M-x)" counsel-M-x)
 ("TAB" "Switch to previous buffer" lesser-evil/switch-to-previous-buffer)
 ("1" "Select window 1" winum-select-window-1)
 ("2" "Select window 2" winum-select-window-2)
 ("2" "Select window 2" winum-select-window-2)
 ("4" "Select window 4" winum-select-window-4)
 ("5" "Select window 5" winum-select-window-5)
 ("6" "Select window 6" winum-select-window-6)
 ("7" "Select window 7" winum-select-window-7)
 ("8" "Select window 8" winum-select-window-8)
 ("9" "Select window 9" winum-select-window-9)
 ("0" "Select window 10" winum-select-window-10))
