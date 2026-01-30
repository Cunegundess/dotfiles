(setq user-full-name "Lucas Cunegundes"
      user-mail-address "lucascsantana6@gmail.com")
(setq doom-font (font-spec :family "Iosevka Nerd Font Mono" :size 15))

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
  
  ;; LSP automático em Python
  (add-hook 'python-mode-hook #'lsp-deferred)
  (add-hook 'python-ts-mode-hook #'lsp-deferred))
(after! lsp-pyright
  (setq lsp-pyright-langserver-command "basedpyright-langserver"
        lsp-pyright-auto-import-completions t
        lsp-pyright-auto-search-paths t
        lsp-pyright-use-library-code-for-types nil
        lsp-pyright-diagnostic-mode "openFilesOnly"
        lsp-pyright-type-checking-mode "off"))

(use-package lsp-mode)
(after! lsp-mode
  (setq lsp-diagnostics-provider :none)
  
  ;; Remove faces de diagnósticos
  (set-face-attribute 'lsp-face-highlight-textual nil :underline nil :background nil)
  (set-face-attribute 'lsp-face-highlight-read nil :underline nil :background nil)
  (set-face-attribute 'lsp-face-highlight-write nil :underline nil :background nil))

;; Flymake também desabilitado
(after! flymake
  (setq flymake-show-diagnostics-at-end-of-line nil
        flymake-indicator-type 'fringe
        flymake-fringe-indicator-position 'right-fringe)
  
  ;; Remove underlines
  (set-face-attribute 'flymake-error nil :underline nil)
  (set-face-attribute 'flymake-warning nil :underline nil)
  (set-face-attribute 'flymake-note nil :underline nil))
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
       :desc "Code actions"        "a" #'lsp-execute-code-action
       :desc "Rename symbol"       "r" #'lsp-rename
       :desc "Format buffer"       "f" #'apheleia-format-buffer
       :desc "Organize imports"    "o" #'lsp-organize-imports
       :desc "Hover doc"           "k" #'lsp-describe-thing-at-point)
      
      ;; Navigation
      (:prefix ("g" . "goto")
       :desc "Definition"          "d" #'lsp-find-definition
       :desc "References"          "r" #'lsp-find-references
       :desc "Implementations"     "i" #'lsp-find-implementation
       :desc "Type definition"     "t" #'lsp-find-type-definition)
      
      ;; Diagnostics (se precisar ativar manualmente)
      (:prefix ("e" . "errors")
       :desc "List workspace errs" "l" #'lsp-treemacs-errors-list
       :desc "Next error"          "n" #'flycheck-next-error
       :desc "Prev error"          "p" #'flycheck-previous-error))
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
