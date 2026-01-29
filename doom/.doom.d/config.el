;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

(setq user-full-name "Lucas Cunegundes"
      user-mail-address "lucascsantana6@gmail.com")

(setq doom-font (font-spec :family "Iosevka Nerd Font Mono" :size 12))

(custom-set-faces!
  '(italic :slant italic)
  '(bold :weight bold)
  '(bold-italic :weight bold :slant italic))

(setq doom-theme 'doom-tokyo-night)
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
;; PYTHON + EGLOT + BASEDPYRIGHT (LIMPO)
;; --------------------
(after! python
  (setq python-shell-virtualenv-root ".venv")

  (add-hook 'python-mode-hook #'eglot-ensure)
  (add-hook 'python-ts-mode-hook #'eglot-ensure)

  (add-hook
   'python-mode-hook
   (lambda ()
     (when (projectile-project-root)
       (setq-local
        eglot-workspace-configuration
        `((:python
           (:analysis
            :typeCheckingMode "off"
            :diagnosticMode "openFilesOnly"
            :autoImportCompletions t
            :autoSearchPaths t
            :extraPaths [,(projectile-project-root)]))
          (:basedpyright
           (:analysis
            :inlayHints
            (:variableTypes nil
             :functionReturnTypes nil
             :callArgumentNames nil
             :genericTypes nil)))))))))

(after! eglot
  (add-to-list 'eglot-server-programs
               '((python-mode python-ts-mode)
                 "basedpyright-langserver" "--stdio"))

  ;; UX limpa
  (setq eglot-autoshutdown t
        eglot-events-buffer-size 0
        eglot-ignored-server-capabilities
        '(:inlayHintProvider
          :documentHighlightProvider
          :documentOnTypeFormattingProvider)))

;; --------------------
;; FLYCHECK (DIAGNÓSTICOS LIMPOS)
;; --------------------
(after! flycheck
  ;; Não pintar o código
  (setq flycheck-highlighting-mode 'symbols
        flycheck-indication-mode nil
        flycheck-checker-error-threshold nil)

  ;; Ruff como único checker
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
              (flycheck-select-checker 'python-ruff)))

  ;; Painel lateral (estilo lsp-ui)
  (setq flycheck-display-errors-function
        #'flycheck-display-error-messages-unless-error-list))

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
  (setq apheleia-mode nil))

;; --------------------
;; KEYBINDS (PADRÃO DOOM)
;; --------------------
(map! :leader
      (:prefix ("c" . "code")
       :desc "Code actions"     "a" #'eglot-code-actions
       :desc "Rename symbol"    "r" #'eglot-rename
       :desc "Format buffer"    "f" #'apheleia-format-buffer
       :desc "Errors list"      "e" #'flycheck-list-errors
       :desc "Go to definition" "d" #'xref-find-definitions
       :desc "References"       "R" #'xref-find-references))

;; --------------------
;; DAPE (DEBUG PYTHON / DOCKER)
;; --------------------
(use-package! dape
  :config
  (setq dape-buffer-window-arrangement 'right
        dape-info-hide-mode-line t)

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
;; KOTLIN
;; --------------------
(use-package! kotlin-mode
  :mode ("\\.kts?\\'" . kotlin-mode))

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
