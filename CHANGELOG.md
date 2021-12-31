# 1.0

## Fixed

- Backwards compatibility with Emacs 26.3 (mainly tested with 29/master)
- Drop the emulate-tab stuff, it doesn't help
- Fix corgi-stateline
- Lots of Corkey improvements and fixes

## Changed

- Split all behavior into Straight.el packages that live under
  `lambdaisland/corgi-packages`
- The default key binding and signal mapping files are now `corgi-keys`,
  `user-keys`, `corgi-signals`, and `user-signals`
- Disable `clj-refactor`. We still include bindings for it, but the user needs
  to opt-in. They will be ignored if the package is not active.
- Enable auto-revert-mode
- Configure common Emoji fonts, so Emoji are more likely to work out of the box
- Disable the audible bell on all systems, not just Mac/Darwin 
- Only load xclip when on terminal
- Bump all dependencies

## Added

- Include a manual
- Include a sample config
- Add CIDER REPL connection indicators (opt-in, call
  `(corgi/enable-cider-connection-indicator)`)
- Commands for jumping to Corgi's/user's key bindings and signal mappings, will
  create a stub one if it does not exist
- `corgi/update-self` for easy updating of Corgi
- `corkey/load-and-watch` : watch user key bindings for changes and reload
- Added a specialized jack-in command for a Babashka utility REPL
- Include `corgi/cider-pprint-eval-register`

# 0.1

## Fixed

* Make more clojure `,` bindings work in the CIDER repl
* require `seq`
* Undo the evil-collection behavior of changing what `last-sexp` means
* Remove `corgi-user-config.el` symlink, instead users should copy the example
  and tweak from there

## Changed

* Changed `jack-in` keybindings to a `,j` prefix. 
  * `,jj` jack in default
  * `,jo` jack in other (cljs)
  * `,ja` jack in all (clj + cljs)
  
## Added

* Started keeping a CHANGELOG
* Added `string-edit` package. No binding yet, use `string-edit-at-point`
* Bindings `gc` for comment-region, `gC` for uncomment-region
* Binding `,sl` for clear-repl
* Binding `SPC w 0` for delete-window (alternative for `SPC w d`)
* Make projectile toggle between test and implemenation create missing tests
