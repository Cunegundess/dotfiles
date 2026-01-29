;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

(setq user-full-name "Lucas Cunegundes"
      user-mail-address "lucascsantana6@gmail.com")

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
;; PYTHON + EGLOT + BASEDPYRIGHT
;; --------------------
(after! python
  ;; Virtualenv padrão do projeto
  (setq python-shell-virtualenv-root ".venv")

  ;; Eglot automático
  (add-hook 'python-mode-hook #'eglot-ensure)
  (add-hook 'python-ts-mode-hook #'eglot-ensure)

  ;; Extra paths úteis (monorepo / docker)
  (add-hook
   'python-mode-hook
   (lambda ()
     (when (projectile-project-root)
       (setq-local
        eglot-workspace-configuration
        `((:python
           (:analysis
            :typeCheckingMode "standard"
            :autoImportCompletions t
            :autoSearchPaths t
            :diagnosticMode "openFilesOnly"
            :extraPaths [,(projectile-project-root)]))
          (:basedpyright
           (:analysis
            :inlayHints
            (:variableTypes t
             :functionReturnTypes t
             :callArgumentNames t
             :genericTypes t)))))))))

(after! eglot
  ;; Pyright server
  (add-to-list 'eglot-server-programs
               '((python-mode python-ts-mode)
                 "basedpyright-langserver" "--stdio"))

  ;; Performance
  (setq eglot-autoshutdown t
        eglot-events-buffer-size 0))

;; --------------------
;; CODE ACTIONS / NAV
;; --------------------
(map! :leader
      "c a" #'eglot-code-actions
      "c r" #'eglot-rename
      "c d" #'xref-find-definitions
      "c R" #'xref-find-references)

;; --------------------
;; APHELEIA + RUFF (FORMAT)
;; --------------------
(use-package! apheleia
  :hook ((python-mode python-ts-mode) . apheleia-mode)
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

;; --------------------
;; FLYCHECK + RUFF (LINT)
;; --------------------
(after! flycheck
  (setq flycheck-checker-error-threshold nil)

  (flycheck-define-checker python-ruff
    "Ruff linter"
    :command ("ruff" "check" "--output-format=text"
              "--stdin-filename" source-original "-")
    :standard-input t
    :error-patterns
    ((error line-start (file-name) ":" line ":" column ": " (message) line-end))
    :modes (python-mode python-ts-mode))

  (add-hook 'python-mode-hook
            (lambda ()
              (flycheck-add-next-checker 'python 'python-ruff))))

;; --------------------
;; DAPE (DEBUG PYTHON / DOCKER)
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
     path-mappings
     (,(lambda ()
         (list
          (cons
           (or (projectile-project-root) default-directory)
           "/app"))))))

  ;; Config fixa (exemplo projeto específico)
  (add-to-list
   'dape-configs
   `(python-docker-alianca
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
