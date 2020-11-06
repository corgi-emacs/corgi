;;; -*- lexical-binding: t -*-

(when (not (server-running-p))
  (server-start))

(set-frame-font "Iosevka Fixed SS14-20")

(global-linum-mode 1)

(add-hook 'before-save-hook
          (lambda ()
            (when (derived-mode-p 'prog-mode)
              (delete-trailing-whitespace))))

(desktop-save-mode)

;; (define-key evil-motion-state-map (kbd "TAB") #'indent-for-tab-command)

(setq cider-repl-display-help-banner nil)
(setq cider-repl-pop-to-buffer-on-connect nil)

(require 'cider)

(defun babashka-quit ()
  (interactive)
  (setq sesman-links-alist
        (a-dissoc sesman-links-alist '(CIDER . "babashka")))
  (when (get-buffer "*babashka-nrepl-server*")
    (kill-buffer "*babashka-nrepl-server*"))
  (when (get-buffer "*babashka-repl*")
    (kill-buffer "*babashka-repl*")))

(defun babashka-jack-in (&optional connected-callback)
  (interactive)
  (babashka-quit)
  (let* ((cmd "bb --nrepl-server")
         (serv-buf (get-buffer-create "*babashka-nrepl-server*"))
         (host "127.0.0.1")
         (repl-builder (lambda (port)
                         (lambda (_)
                           (let ((repl-buf (get-buffer-create "*babashka-repl*")))
                             (with-current-buffer repl-buf
                               (cider-repl-create (list :repl-buffer repl-buf
                                                        :repl-type 'clj
                                                        :host host
                                                        :port port
                                                        :project-dir "~"
                                                        :session-name "babashka"
                                                        :repl-init-function (lambda ()
                                                                              (setq-local cljr-suppress-no-project-warning t
                                                                                          cljr-suppress-middleware-warnings t)
                                                                              (rename-buffer "*babashka-repl*")))))))))
         (port-filter (lambda (serv-buf)
                        (lambda (process output)
                          (when (buffer-live-p serv-buf)
                            (with-current-buffer serv-buf
                              (insert output)
                              (when (string-match "Started nREPL server at 127.0.0.1:\\([0-9]+\\)" output)
                                (let ((port (string-to-number (match-string 1 output))))
                                  (setq nrepl-endpoint (list :host host :port port))
                                  (let ((client-proc (nrepl-start-client-process
                                                      host
                                                      port
                                                      process
                                                      (funcall repl-builder port))))
                                    (set-process-query-on-exit-flag client-proc nil)
                                    (when connected-callback
                                      (funcall connected-callback client-proc)))))))))))
    (with-current-buffer serv-buf
      (setq nrepl-is-server t
            nrepl-server-command cmd))
    (let ((serv-proc (start-file-process-shell-command "babashka-nrepl-server" serv-buf cmd)))
      (set-process-query-on-exit-flag serv-proc nil)
      (set-process-filter serv-proc (funcall port-filter serv-buf))
      (set-process-sentinel serv-proc 'nrepl-server-sentinel)
      (set-process-coding-system serv-proc 'utf-8-unix 'utf-8-unix)))
  nil)

(set-register ?k "(do (require 'kaocha.repl) (kaocha.repl/run))")
(set-register ?K "(do (require 'kaocha.repl) (kaocha.repl/run-all))")
(set-register ?r "(user/refresh)")
(set-register ?g "(user/go)")
(set-register ?b "(user/browse)")

(defun lesser-evil/cider-modeline-info ()
  (when (derived-mode-p 'clojure-mode)
    (let ((source-project-name (projectile-project-name)))
      (if-let* ((repls (ignore-errors (cider-repls (cider-repl-type-for-buffer)))))
          (thread-last repls
            (seq-map
             (lambda (repl)
               (with-current-buffer repl
                 (if (equal (buffer-name repl) "*babashka-repl*")
                     (propertize "bb" 'face '( :background "green"
                                               :foreground "black"))
                   (let ((info (concat
                                (when-let ((repl-project-name (cider--project-name nrepl-project-dir)))
                                  (when (not (equal repl-project-name source-project-name))
                                    (concat ":" repl-project-name)))
                                (pcase (plist-get nrepl-endpoint :host)
                                  ("localhost" "")
                                  ("127.0.0.1" "")
                                  (x (concat ":" x)))
                                ;;(format ":%d" (plist-get nrepl-endpoint :port))
                                )))
                     (cl-case cider-repl-type
                       (clj (propertize (concat "clj" info) 'face '( :background "#5881D8"
                                                                     :foreground "white")))
                       (cljs (propertize (concat "cljs" info) 'face '( :background "#f7df1e"
                                                                       :foreground "black")))
                       (pending-cljs (propertize (concat "pending-cljs" info) 'face '( :background "#f7df1e"
                                                                                       :foreground "black")))))))))
            (s-join " "))
        (propertize "<not connected>" 'face '( :background "red"
                                               :foreground "white"))))))

(add-hook 'clojure-mode-hook (lambda ()
                               (setq mode-line-format
                                     '("%e" (:eval (format winum-format (winum-get-number-string)))
                                       mode-line-front-space
                                       mode-line-mule-info
                                       mode-line-client
                                       mode-line-modified
                                       mode-line-remote
                                       mode-line-frame-identification
                                       mode-line-buffer-identification
                                       "   "
                                       mode-line-position
                                       evil-mode-line-tag
                                       (vc-mode vc-mode)
                                       " "
                                       (:eval (lesser-evil/cider-modeline-info))
                                       " "
                                       mode-line-modes
                                       mode-line-misc-info
                                       mode-line-end-spaces))))


(define-clojure-indent
  (DELETE 2)
  (GET 2)
  (POST 2)
  (PUT 2)
  (assoc 0)
  (async nil)
  (at 1)
  (await 1)
  (case-of 2)
  (catch-pg-key-error 1)
  (context 2)
  (defplugin '(1 :form (1)))
  (element 2)
  (ex-info 0)
  (filter-routes 1)
  (handle-pg-key-error 2)
  (js/React.createElement 2)
  (match 1)
  (promise 1)
  (prop/for-all 1)
  (s/fdef 1))

(use-package auto-highlight-symbol)

(with-current-buffer (get-buffer "*scratch*")
  (erase-buffer)
  (insert ";; This is a scratch buffer for temporary code and trying things out.
;; You can evaluate Clojure code here with , e e
")
  (goto-char (point-max))
  (dolist (window (get-buffer-window-list "*scratch*"))
    (set-window-point window (point-max))))

(babashka-jack-in
 (lambda (_)
   (sesman-link-session 'CIDER '("babashka") 'buffer (get-buffer "*scratch*"))))
