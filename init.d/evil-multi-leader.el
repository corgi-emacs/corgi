(require 'evil)
;; Multi leader mode (really a generic keybinding lib for evil + which-key)

(require 'evil)
(require 'which-key)

(defvar eml/map (make-sparse-keymap))
(defvar eml/binding-files (list (expand-file-name "leader_bindings2.el" user-emacs-directory)))

(defun eml/read-file (file-name)
  (with-temp-buffer
    (insert-file-contents file-name)
    (goto-char (point-min))
    (read (current-buffer))))

(defun eml/kv-list-get (list key)
  (while (and list (not (eql key (car list))))
    (setq list (cddr list)))
  (cadr list))

(defun eml/flatten-bindings (prefix bindings)
  (let ((p (car bindings))
        (desc (cadr bindings))
        (rest (cddr bindings)))
    (if (symbolp (car rest))
        (list (list (concat prefix p) desc (car rest)))
      (cl-list* (list (concat prefix p) desc)
                (seq-mapcat (lambda (b)
                              (eml/flatten-bindings (concat prefix p " ") b))
                            rest)))))

(defun eml/set-bindings* (keymap bindings command-mapping)
  (setcdr keymap nil)
  (cl-loop
   for binding-set in bindings
   do
   (cl-loop
    for (keys desc cmd) in (eml/flatten-bindings "" binding-set)
    do
    (cond
     ((keywordp cmd)
      (let ((sym (eml/kv-list-get command-mapping cmd)))
        (when sym
          (when (listp sym)
            (setq desc (car sym))
            (setq sym (cadr sym)))
          (progn
            ;; We stick these in the map for the current major mode, since these
            ;; are major mode specific, so having them in our shared minor mode
            ;; map causes issues. The downside here is it makes it harder to
            ;; clean things up when the minor mode gets disabled. Not a huge
            ;; issue for me right now but eventually we'll need a better
            ;; approach, possibly adding to minor-mode-map-alist with synthetic
            ;; "minor mode" variables each corresponding with a certain major
            ;; mode
            (evil-define-key 'normal (current-local-map) (kbd keys) sym)
            (evil-define-key 'motion (current-local-map) (kbd keys) sym)))))
     ((symbolp cmd)
      (progn
        (evil-define-key 'normal keymap (kbd keys) cmd)
        (evil-define-key 'motion keymap (kbd keys) cmd))))
    (which-key-add-key-based-replacements keys desc))))

(defun eml/set-bindings (binding-spec)
  ;;(message "---> bindings for %s %s" major-mode (buffer-name))
  (let ((bindings (eml/kv-list-get binding-spec :bindings))
        (modes (eml/kv-list-get binding-spec :modes))
        (commands))
    (cl-loop
     for (mode mapping) in modes
     do
     (when (derived-mode-p mode)
       (setq commands mapping)))
    (eml/set-bindings* eml/map bindings commands))
  (evil-normalize-keymaps))

(defun eml/maybe-set-bindings ()
  (interactive)
  (when multi-leader-mode
    (cl-loop
     for file in eml/binding-files
     do
     (eml/set-bindings (eml/read-file file)))))

(define-minor-mode multi-leader-mode
  "Multi leader mode"
  :keymap eml/map
  ;; this needs fine tuning, but it already skips a lot of internal buffers like
  ;; *nrepl-decoding*
  (when (not (or  (eql major-mode 'fundamental-mode)
                  (eql major-mode 'minibuffer-inactive-mode)))
    ;;(message "multi-leader-mode (minor mode) %s" (buffer-name))
    (eml/maybe-set-bindings)))

;;(eml/set-bindings (eml/read-file (car eml/binding-files)))

(define-minor-mode global-multi-leader-mode
  "Global minor mode for multi-leader support."
  nil nil nil
  (if global-multi-leader-mode
      (progn
        ;;(add-hook 'evil-local-mode-hook #'multi-leader-mode)
        (add-hook 'after-change-major-mode-hook #'multi-leader-mode)
        (multi-leader-mode 1))

    (multi-leader-mode -1)
    (remove-hook 'after-change-major-mode-hook #'multi-leader-mode)
    ;;(remove-hook 'evil-local-mode-hook #'multi-leader-mode)
    ))

(provide 'evil-multi-leader)
