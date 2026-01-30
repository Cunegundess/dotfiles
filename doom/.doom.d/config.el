(setq user-full-name "Lucas Cunegundes"
      user-mail-address "lucascsantana6@gmail.com")
(setq doom-font (font-spec :family "Iosevka Nerd Font Mono" :size 12))

(custom-set-faces!
  '(italic :slant italic)
  '(bold :weight bold)
  '(bold-italic :weight bold :slant italic))
(setq doom-theme 'doom-tokyo-night
      display-line-numbers-type 'relative)

(beacon-mode 1)
(use-package! org
  :config
  (setq org-directory "~/Documentos/"
        org-agenda-files '("~/Documentos/agenda.org")
        org-log-done 'time)
  (add-hook 'org-mode-hook #'org-bullets-mode))
(after! python
  (setq python-shell-virtualenv-root ".venv")
  (add-hook 'python-mode-hook #'eglot-ensure)
  (add-hook 'python-ts-mode-hook #'eglot-ensure))
(after! eglot
  (setq eglot-autoshutdown t
        eglot-sync-connect nil
        eglot-extend-to-xref t
        eglot-report-progress nil
        eglot-events-buffer-size 0)
  
  ;; BasedPyright como LSP server
  (add-to-list 'eglot-server-programs
               '((python-mode python-ts-mode)
                 . ("basedpyright-langserver" "--stdio")))
  
  ;; Workspace configuration - Django optimized
  (defun +python-eglot-workspace-config ()
    "Configuração BasedPyright otimizada para Django/Docker."
    (when (projectile-project-root)
      `(:basedpyright
        (:analysis
         (:typeCheckingMode "off"
          :diagnosticMode "openFilesOnly"
          :autoImportCompletions t
          :autoSearchPaths t
          :useLibraryCodeForTypes nil
          :extraPaths ["."]
          :diagnosticSeverityOverrides
          (:reportUnknownMemberType "none"
           :reportUnknownVariableType "none"
           :reportUnknownArgumentType "none"
           :reportUnknownParameterType "none"
           :reportMissingTypeStubs "none"
           :reportGeneralTypeIssues "none"
           :reportOptionalMemberAccess "none"
           :reportOptionalSubscript "none"
           :reportPrivateImportUsage "none"
           :reportAttributeAccessIssue "none"
           :reportIncompatibleMethodOverride "none"))))))
  
  (add-hook 'python-mode-hook
            (lambda ()
              (setq-local eglot-workspace-configuration
                          (+python-eglot-workspace-config))))
  
  ;; Remove símbolos ! inline
  (add-hook 'eglot-managed-mode-hook
            (lambda ()
              (setq-local eldoc-documentation-strategy
                          'eldoc-documentation-compose-eagerly))))
(after! flymake
  ;; Configuração visual
  (setq flymake-show-diagnostics-at-end-of-line nil
        flymake-indicator-type 'fringe
        flymake-fringe-indicator-position 'right-fringe
        flymake-no-changes-timeout 0.5
        flymake-start-on-save-buffer t)
  
  ;; Remove todos os underlines
  (set-face-attribute 'flymake-error nil :underline nil)
  (set-face-attribute 'flymake-warning nil :underline nil)
  (set-face-attribute 'flymake-note nil :underline nil)
  
  ;; Bitmaps minimalistas para a fringe
  (define-fringe-bitmap 'flymake-error-bitmap
    [#b00000000
     #b00011000
     #b00011000
     #b00000000] nil nil 'center)
  
  (define-fringe-bitmap 'flymake-warning-bitmap
    [#b00000000
     #b00011000
     #b00011000
     #b00000000] nil nil 'center))
(use-package! apheleia
  :hook ((python-mode python-ts-mode) . apheleia-mode)
  :config
  ;; Usa Ruff como formatador padrão
  (setf (alist-get 'python-mode apheleia-mode-alist) '(ruff-isort ruff))
  (setf (alist-get 'python-ts-mode apheleia-mode-alist) '(ruff-isort ruff))
  
  ;; Configuração dos formatadores
  (setf (alist-get 'ruff apheleia-formatters)
        '("ruff" "format" "--stdin-filename" filepath))
  (setf (alist-get 'ruff-isort apheleia-formatters)
        '("ruff" "check" "--select" "I" "--fix" "--stdin-filename" filepath)))
(use-package! dape
  :config
  (setq dape-buffer-window-arrangement 'right
        dape-info-hide-mode-line t)

  ;; Config genérica para qualquer projeto Docker
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

  ;; Config específica - Projeto Aliança
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
(map! :leader
      ;; Code actions
      (:prefix ("c" . "code")
       :desc "Code actions"        "a" #'eglot-code-actions
       :desc "Rename symbol"       "r" #'eglot-rename
       :desc "Format buffer"       "f" #'apheleia-format-buffer
       :desc "Hover doc"           "k" #'eldoc-doc-buffer)
      
      ;; Navigation
      (:prefix ("g" . "goto")
       :desc "Definition"          "d" #'xref-find-definitions
       :desc "References"          "r" #'xref-find-references
       :desc "Implementations"     "i" #'eglot-find-implementation)
      
      ;; Diagnostics
      (:prefix ("e" . "errors")
       :desc "Next diagnostic"     "n" #'flymake-goto-next-error
       :desc "Prev diagnostic"     "p" #'flymake-goto-prev-error
       :desc "Buffer diagnostics"  "l" #'flymake-show-buffer-diagnostics
       :desc "Project diagnostics" "L" #'flymake-show-project-diagnostics))
(use-package! ejc-sql
  :commands (ejc-sql-mode ejc-sql-connect)
  :config
  (setq ejc-sql-separator ";"
        ejc-use-flx t
        ejc-result-table-impl 'ejc-result-table-tabulated))

(after! ejc-sql
  ;; Conexão - Projeto Nexus
  (ejc-create-connection
   "postgres-nexus"
   :classpath (ejc-find-postgres-jdbc)
   :dbtype "postgresql"
   :dbname "nexus_rfid"
   :host "localhost"
   :port 5432
   :user "postgres")

  ;; Conexão - Projeto Aliança
  (ejc-create-connection
   "postgres-alianca"
   :classpath (ejc-find-postgres-jdbc)
   :dbtype "postgresql"
   :dbname "alianca_rfid"
   :host "localhost"
   :port 5432
   :user "postgres"))
(use-package! vterm
  :commands vterm
  :config
  (setq vterm-shell "/bin/zsh"))
