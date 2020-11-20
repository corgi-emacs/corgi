# pre-release

## Fixed

* Make more clojure `,` bindings work in the CIDER repl
* require `seq`
* Undo the evil-collection behavior of changing what `last-sexp` means

## Changed

* Changed `jack-in` keybindings to a `,j` prefix. 
  * `,jj` jack in default
  * `,jo` jack in other (cljs)
  * `,ja` jack in all (clj + cljs)
  
## Added

* Added `string-edit` package. No binding yet, use `string-edit-at-point`
* Bindings `gc` for comment-region, `gC` for uncomment-region
* Binding `,sl` for clear-repl
* Binding `SPC w 0` for delete-window (alternative for `SPC w d`)
* Make projectile toggle between test and implemenation create missing tests
