# Corgi

A minimalistic evil-based Emacs configuration that feels like a slimmed down
version of Spacemacs.

**This is a work in progress, please get in touch if you want to help beta-test
Corgi.**

We've started on a [User Manual](corgi_manual.org), please start there.

## Motivation

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

Corgi comes with its own keybinding package, `corkey`, which sets up bindings
based on one or more keybinding files. To customize, either add keybinding files
into your config which get layered over the built-in ones, or copy the corgi
bindings file into your config and customize it.

Corkey uses an extra layer of indirection called signals, this makes it easy to
have consistent key bindings across major-modes. E.g. Corgi provides the same
shortcuts for evaluating code, no matter in which language.

It also makes it easier to provide alternative binding sets, while honoring
people's preferences. E.g. a binding set can have `Ctrl-F` for `:search/buffer`,
which may invoke `swiper` or `isearch` or something else, based on the user's
preferences.

## Principles

**Everything is a package**

Corgi itself is just a collection of packages, and most of what Corgi does is
pull in a selection of packages.

Custom functions and modifications go into their own dedicated package, so they
are available for use by the community at large, or they are submitted upstream,
so that the config itself is just loading packages and tying them together.

**It's your config**

We don't take over `init.el`, at the end of the day you decide how your config
works. You decide which Corgi packages to pull in, and if you don't like some of
our choices, you can simply copy over the relevant code and tweak it from there.

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

**Reproducible**

Everything is immutably versioned with straight. People should never run into
issues because of slightly different package versions.

**Somewhat opinionated**

This config makes a few clear, non-negotiable decisions. It's evil based, has
structural editing, etc. But on some things it does not have
an opinion. 

## License

GPL version 3

Copyright &copy; Arne Brasseur 2020-2021
