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

(defun eml/set-bindings* (bindings command-mapping)
  (setcdr eml/map nil)
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
            (evil-define-key 'normal eml/map (kbd keys) sym)
            (evil-define-key 'motion eml/map (kbd keys) sym)
            ))))
     ((symbolp cmd)
      (progn
        (evil-define-key 'normal eml/map (kbd keys) cmd)
        (evil-define-key 'motion eml/map (kbd keys) cmd))))
    (which-key-add-key-based-replacements keys desc))))

(defun eml/set-bindings (binding-spec)
  (let ((bindings (eml/kv-list-get binding-spec :bindings))
        (modes (eml/kv-list-get binding-spec :modes))
        (commands))
    (cl-loop
     for (mode mapping) in modes
     do
     (when (derived-mode-p mode)
       (setq commands mapping)))
    (eml/set-bindings* bindings commands))
  (evil-normalize-keymaps))

(define-minor-mode multi-leader-mode
  "Multi leader mode"
  :keymap eml/map
  (when multi-leader-mode
    (cl-loop
     for file in eml/binding-files
     do
     (eml/set-bindings (eml/read-file file)))))

;;(eml/set-bindings (eml/read-file (car eml/binding-files)))

(define-minor-mode global-multi-leader-mode
  "Global minor mode for multi-leader support."
  nil nil nil
  (if global-multi-leader-mode
      (progn
        (add-hook 'evil-local-mode-hook #'multi-leader-mode t)
        (multi-leader-mode 1))
    
    (multi-leader-mode -1)
    (remove-hook 'evil-local-mode-hook #'multi-leader-mode t)))

(provide 'evil-multi-leader)
