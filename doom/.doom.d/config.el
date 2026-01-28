;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

(setq user-full-name "Lucas Cunegundes"
      user-mail-address "lucascsantana6@gmail.com")

;; (setq doom-font (font-spec :family "Iosevka Nerd Font Mono" :size 12))
(setq doom-font (font-spec :family "JetBrains Mono Nerd Font" :size 12))

(custom-set-faces!
  '(italic :slant italic)
  '(bold :weight bold)
  '(bold-italic :weight bold :slant italic))

(setq doom-theme 'doom-meltbus)
(setq display-line-numbers-type 'relative)
(beacon-mode 1)

;; --------------------
;; ORG
;; --------------------
(use-package! org
  :config
  (setq org-directory "~/Documentos/"
        org-agenda-files '("~/Documentos/agenda.org")
        org-log-done 'time)
  (add-hook 'org-mode-hook #'org-bullets-mode))

;; --------------------
;; LSP / PYTHON / RUFF
;; --------------------
(after! lsp-mode
  (setq lsp-idle-delay 0.5
        lsp-log-io nil
        lsp-enable-file-watchers nil
        lsp-file-watch-threshold 5000
        lsp-disabled-clients '(pylsp))

  ;; UI / Code actions
  (setq lsp-ui-sideline-enable t
        lsp-ui-doc-enable t))

(after! flycheck
  (setq flycheck-checker-error-threshold nil))

(after! lsp-pyright
  (setq lsp-pyright-langserver-command "basedpyright"
        lsp-pyright-type-checking-mode "standard"
        lsp-pyright-auto-import-completions t
        lsp-pyright-auto-search-paths t
        lsp-pyright-diagnostic-mode "openFilesOnly"
        ;; lsp-pyright-disable-organize-imports t
        ;; lsp-pyright-use-library-code-for-types t
        lsp-pyright-venv-path "."))

(after! python
  (setq python-shell-virtualenv-root ".venv")
  (add-hook 'python-mode-hook #'lsp-deferred)
  (add-hook 'python-mode-hook
            (lambda ()
              (when (projectile-project-root)
                (setq-local lsp-pyright-extra-paths
                            (vector (projectile-project-root)))))))

;; --------------------
;; APHELEIA + RUFF
;; --------------------
(use-package! apheleia
  :hook (python-mode . apheleia-mode)
  :config
  (setf (alist-get 'python-mode apheleia-mode-alist) 'ruff)
  (setf (alist-get 'python-ts-mode apheleia-mode-alist) 'ruff)
  (setf (alist-get 'ruff apheleia-formatters)
        '("ruff" "format" "--stdin-filename" filepath))
  (setq apheleia-mode nil)
  (map! :map python-mode-map
        :localleader
        "f" #'apheleia-format-buffer
        "F" #'apheleia-format-region))

(after! flycheck
  (flycheck-define-checker python-ruff
    "Ruff linter"
    :command ("ruff" "check" "--output-format=text" "--stdin-filename" source-original "-")
    :standard-input t
    :error-patterns
    ((error line-start (file-name) ":" line ":" column ": " (message) line-end))
    :modes (python-mode python-ts-mode))

  (add-hook 'lsp-managed-mode-hook
            (lambda ()
              (when (derived-mode-p 'python-mode 'python-ts-mode)
                (flycheck-add-next-checker 'lsp 'python-ruff)))))

(map! :leader "c a" #'lsp-execute-code-action)

;; --------------------
;; DAP PYTHON
;; --------------------
;; (after! dap-mode
;;   (dap-mode 1)
;;   (dap-auto-configure-mode 1))

;; (after! dap-ui
;;   (dap-ui-mode 1))

;; (use-package! dap-python
;;   :after dap-mode
;;   :config
;;   (setq dap-python-debugger 'debugpy)

;;   (defun my/project-root ()
;;     (or (projectile-project-root) default-directory))

;;   (dap-register-debug-template
;;    "Python Docker"
;;    (list
;;     :type "python"
;;     :request "attach"
;;     :name "Python Docker"
;;     :connect (list :host "127.0.0.1" :port 5678)
;;     :pathMappings
;;     (vector
;;      (list
;;       :localRoot (my/project-root)
;;       :remoteRoot "/app"))
;;     :django t
    ;; :justMyCode :json-false)))

;; --------------------
;; DAPE (DEBUG)
;; --------------------
(use-package! dape
  :config
  (setq dape-buffer-window-arrangement 'right
        dape-info-hide-mode-line t)

  ;; Python Docker attach (debugpy)
  (add-to-list
   'dape-configs
   `(python-docker
     modes (python-mode python-ts-mode)
     command "python"
     command-args ("-m" "debugpy.adapter")
     port 5678
     host "127.0.0.1"
     cwd ,(lambda () (or (projectile-project-root) default-directory))
     path-mappings (,(lambda ()
                       (list
                        (cons
                         (or (projectile-project-root) default-directory)
                         "/app"))))))

  (add-to-list
   'dape-configs
   `(python-docker
     modes (python-mode python-ts-mode)
     command "python"
     command-args ("-m" "debugpy.adapter")
     port 5678
     host "127.0.0.1"
     cwd "/home/lucas/Projects/work/alianca/apps/backend/"
     path-mappings
     (("/home/lucas/Projects/work/alianca/apps/backend/" . "/app")))))

;; --------------------
;; DATABASE (EJC-SQL)
;; --------------------
(use-package! ejc-sql
  :commands ejc-sql-mode ejc-sql-connect
  :config
  (setq ejc-sql-separator ";"
        ejc-use-flx t
        ejc-result-table-impl 'ejc-result-table-tabulated))

(after! ejc-sql
  (ejc-create-connection
   "postgres-nexus"
   :classpath (ejc-find-postgres-jdbc)
   :dbtype "postgresql"
   :dbname "nexus_rfid"
   :host "localhost"
   :port 5432
   :user "postgres")

  (ejc-create-connection
   "postgres-alianca"
   :classpath (ejc-find-postgres-jdbc)
   :dbtype "postgresql"
   :dbname "alianca_rfid"
   :host "localhost"
   :port 5432
   :user "postgres"))

;; --------------------
;; VTERM
;; --------------------
(use-package! vterm
  :commands vterm
  :config
  (setq vterm-shell "/bin/zsh"))
