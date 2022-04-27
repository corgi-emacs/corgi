# Corgi

<img align="right" width="100" src="logo/corgi_icon.webp">

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

There is documentation in the form of a [User Manual](corgi_manual.org), we
recommend reading it to get an idea about what is there.

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

GPL version 3

Copyright &copy; Arne Brasseur 2020-2022
