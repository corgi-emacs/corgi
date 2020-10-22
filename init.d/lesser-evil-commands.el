;; Commands that are not available in vanilla emacs, and that are not worth
;; pulling in a separate package to provide them. These should eventually end up
;; in their own utility package, we do not want too much of this stuff directly
;; in the emacs config.

(defun lesser-evil/switch-to-previous-buffer ()
  "Switch to previously open buffer.
Repeated invocations toggle between the two most recently open buffers."
  (interactive)
  (switch-to-buffer (other-buffer (current-buffer) 1)))

(defun lesser-evil/cider-toggle-repl ()
  (interactive)
  (if (eq m 'cider-repl-mode)
      (cider-switch-to-last-clojure-buffer)
    (cider-switch-to-repl-buffer)))

(defun lesser-evil/double-columns ()
  "Simplified version of spacemacs/window-split-double-column"
  (interactive)
  (delete-other-windows)
  (let* ((previous-files (seq-filter #'buffer-file-name
                                     (delq (current-buffer) (buffer-list)))))
    (set-window-buffer (split-window-right)
                       (or (car previous-files) "*scratch*"))
    (balance-windows)))

(defun lesser-evil/cider-pprint-register (register)
  (interactive (list (register-read-with-preview "Eval register: ")))
  (cider--pprint-eval-form (get-register register)))

(provide 'lesser-evil-commands)
