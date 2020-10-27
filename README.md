# Lesser Evil

A minimalistic evil-based Emacs configuration that feels like a slimmed down version of Spacemacs.

**This is a personal setup and a work in progress. Things will still change, some things will seem highly opinionated, some things will still break. Use at your own risk. The plan is to eventually evolve this into a @lambdaisland endorsed Clojure-focused evil-based Emacs config, but until we get there this is not intended for general consumption.**

**Initial write-up of the first version is below, but is already outdated.**

----

What's in the box so far?

- evil
- evil-collection
- evil-leader
- ivy
- counsel
- swiper
- xclip
- which-key
- winum
- undo-fu
- clojure/cider/clj-refactor
- aggressive-indent
- company
- projectile
- magit

While I love Spacemacs I find the sheer size of it can become an issue. It
contains much functionality and many customizations that I don't need, and I
struggle to understand how things fit together, or how to add my own
customizations.

This is an attempt to go back to the basics, a simple and straightforward Emacs
config built from first principles, using `straight.el` for reproducible package
installs, and `use-package` to keep the config tidy. It sets up Vim/Evil style
editing, as well as spacebar-prefixed keybindings in the style of Spacemacs.

I especially felt the need for a lighter config when doing [remote pairing using
tmux](https://lambdaisland.com/blog/2019-12-12-advent-of-parens-12-pairing-cloud-tmux).
The limited set of dependencies should make this config quick to set up, and
snappy in use.

Keybindings based on a `SPC` leader key are installed based on
`leader_bindings.el`. This provides a declarative single source of truth for all
Spacemacs-like bindings.

``` emacs-lisp
(("b" "Buffer commands"
  ("b" "Switch buffer" ivy-switch-buffer)
  ("d" "Kill buffer" kill-buffer))

 ("f" "File commands"
  ("f" "Find file" counsel-find-file)
  ("s" "Save file" save-buffer)
  ("S" "Save all" evil-write-all))

 ("s" "Search commands"
  ("s" "Search in buffer" swiper))

 ("h" "Help"
  ("d" "Describe"
   ("k" "Describe key" describe-key)
   ("v" "Describe variable" counsel-describe-variable)
   ("f" "Describe function" counsel-descbinds-function)))

 ("w" "Windows"
  ("1" "Delete other windows" delete-other-windows)
  ("/" "Split window right" split-window-right)
  ("-" "Split window below" split-window-below)
  ("o" "Go to other window" other-window)
  ("d" "Delete window" delete-window))

 ("SPC" "Execute command (M-x)" counsel-M-x)
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
```

## Principles

**Everything is a package**

Custom functions and modifications should become their
own package, or be submitted upstream, so that the config itself is just loading
packages and tying them together.

-> still a few things in here that need to go into packages.

**When in doubt, do less**

When there are multiple competing options for how to handle something, do the
one that is closer to the default.

**Declarative keybindings**

Bindings are data and should be in one place, people who want to tweak or supply
their own bindings need to just edit one file with bindings and nothing but
bindings.

**Consistent keybindings**

When a command is specific to a major mode, its binding should do the closest
equivalent in other major modes, or do nothing. So no bindings that do radically
different things depending on the mode. (this is mostly for `,` leader keys)

**A good enough subset**

We don't want to do everything that Spacemacs (or vim) does, we do want to have
a good enough set of commands that a person can easily learn and be productive
with, without getting overwhelmed.

**Terminal-first**

Things should work just as well in the terminal as in the GUI. There should be
no bindings or behaviors that only work in the GUI.

## License

GPL version 3

Copyright &copy; Arne Brasseur 2020
