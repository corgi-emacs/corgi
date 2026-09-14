# Corgi

<img align="right" width="200" src="logo/corgi_icon.webp">

Corgi is a collection of Emacs packages, allowing you to simplify your own Emacs
configuration. It is compatible with Emacs 29 and later.

If you load all of them, you get a fairly opinionated, but minimal, Evil-based
(vim-style) editor experience, reminiscent of Spacemacs, with particular
affordances for web development and for Lisp-style languages. But you can also
just load the few packages that appeal to you, ignore the rest, and build your
own config from there.

With Corgi, you stay in control of your own Emacs config. We merely provide some
building blocks, so you don't need to reinvent the wheel. Packages are
elaborately commented, so you can also just use them as inspiration.

You can find the packages at
[github.com/corgi-emacs/corgi-packages](https://github.com/corgi-emacs/corgi-packages),
which is set up as a package repository for the
[Straight](https://github.com/raxod502/straight.el) package manager (you can
also just grab them manually or through some other packaging system that
understands git).

## Getting Started

To get started, download `bootstrap.el`, this sets up Straight and
`corgi-packages`.

```shell
mkdir -p ~/.emacs.d
curl -sL https://github.com/corgi-emacs/corgi/blob/main/bootstrap.el -o ~/.emacs.d/bootstrap.el
```

A basic Corgi `~/.emacs.d/init.el` then becomes:

```emacs-lisp
(load-file (expand-file-name "bootstrap.el" user-emacs-directory))

(use-package corgi-defaults)

(use-package corgi-evil)
(use-package corgi-editor)
(use-package corgi-commands)
(use-package corgi-bindings)

(use-package corgi-completion-ui)
(use-package corgi-stateline)

(use-package corgi-clojure)
(use-package corgi-emacs-lisp)

(use-package corkey
  :config
  (corkey-mode 1)
  (corkey/load-and-watch))
```

## Packages

Corgi offers four types of packages: 

- Defaults package: configures various built-in emacs flags and behaviors (`corgi-defaults`)
- Meta packages: load other packages (through use-package) and configures them (`corgi-evil`, `corgi-editor`, `corgi-emacs`, etc)
- Feature packages: standalone pieces of functionality, typically provided as a minor-mode (`corkey`, `corgi-evil-colorize-modeline`, etc)
- Keybindings: binding and signal definitions for our keybinding layer, `corkey`

### corgi-defaults

**Type: defaults package**

This tweaks a bunch of Emacs settings, and enables certain minor modes. Some are
basic quality of life things, like pixel-level scrolling, or showing trailing
blank lines in the fringe; some are power-user settings, like disabling the menu
and toolbar, preferring a simply "y"/"n" instead "yes"/"no", and disabling
shift-select.

This package is quite opinionated, if you don't like some of these settings,
either change them after loading the package (with `setq` or `(some-minor-mode
-1)`), or copy it to your own config (e.g. `~/.emacs.d/defaults.el`) and tweak
it from there.

### corgi-evil

**Type: meta package**

Set up Evil (vim-style editing), and some related packages and opinionated
defaults.

This also loads
[`evil-collection`](https://github.com/emacs-evil/evil-collection), basically a
giant set of crowdsourced keybindings to make many parts of Emacs more vim-like.

We add [`evil-surround`](https://github.com/emacs-evil/evil-surround), which
provides similar functionality to the
[surround.vim](https://github.com/tpope/vim-surround) package by Tim Pope. This
adds vim-style commands that work on surrounding delimiters.

We deviate from the Vim-style cursor behavior, when exiting insert mode the
cursor does not jump back, and you are able to move the cursor one character
beyond the end of a line. If you are new to modal editing then this should be
more intuitive behavior, but if you're already steaped in Vim and prefer that
style, reset these variables:

```emacs-lisp
(use-package corgi-evil
  :config
  (setq evil-move-cursor-back t
        evil-move-beyond-eol nil))
```

Finally `corgi-evil` contains a hook to change back to default mode, whenever
you change buffers.

### corgi-evil-colorize-modeline

**Type: feature package**

This package is part of `corgi-evil`:

```emacs-lisp
(use-package corgi-evil-colorize-modeline
  :straight corgi-evil
  :config
  (global-corgi-evil-colorize-modeline-mode 1))
```

It changes the background of the Emacs modeline, based on the Evil-mode state
(insert, visual, emacs, etc). Colors can be customized.



----

Corgi is an Emacs configuration for Clojure developers who like Vim-style modal
editing.

The UX is heavily inspired by Spacemacs (similar `SPC` and `,` leader key
bindings), but is about 50x less code. That makes it faster to start up,
spiffier in use, and a lot more pleasant to deal with when things go wrong.

Corgi is an _unbundled_ Emacs config. Instead of providing a full config we
provide a set of packages (see
[corgi-packages](https://github.com/lambdaisland/corgi-packages)) for use with
[Straight.el](https://github.com/raxod502/straight.el) package manager. The
Emacs config itself (the contents of `~/.emacs.d`) are **yours**. We provide a
[sample-config](https://github.com/lambdaisland/corgi/tree/main/sample-config).

If you're impatient then just copy the contents of that directory to
`~/.emacs.d` and start Emacs. The first run will need some time to install
various packages, after that you're ready to go. Try `SPC f e K` to see all
Corgi-specific key bindings.

## Getting started

There is documentation in the form of a [User Manual](corgi_manual.org), we
recommend reading it to get an idea about what is there.

There is a intro video guide to Corgi by [@oxalorg](https://github.com/oxalorg)
here: [youtube.com/watch?v=5q4UmX45ZlM](https://www.youtube.com/watch?v=5q4UmX45ZlM)

## Status

After two years of development we've decided to call it 1.0. It's ready, go use
it! Issue reports are very welcome, as are PRs, although it's always a good idea
to sollicit feedback first via an issue, to make sure your changes are in line
with the scope and philosophy of the project.

## Corkey

The bulk of Corgi is just a selection of packages, with sprinkles of
configuration and glue. The big exception is Corkey, this is our key binding
layer, which builds on top of Evil, but does things in its own unique way,
different from what you may be used to from Emacs, Evil, or Spacemacs.

The [User Manual](corgi_manual.org) explains in more depth what Corkey is, how
it works, and why it does things in a certain way. To make good use of Corgi we
recommend getting familiar with Corkey.

## License

Licensed under GNU General Public License, version 3

Copyright &copy; Arne Brasseur 2020-2026
