(setq user-full-name "Lucas Cunegundes"
      user-mail-address "lucascsantana6@gmail.com")
(setq doom-font (font-spec :family "Iosevka Nerd Font Mono" :size 14))

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
  (add-hook 'python-mode-hook #'lsp-deferred)
  (add-hook 'python-ts-mode-hook #'lsp-deferred))

(after! lsp-pyright
  (setq lsp-pyright-langserver-command "basedpyright-langserver"
        lsp-pyright-type-checking-mode "basic"
        lsp-pyright-diagnostic-mode "openFilesOnly"
        
        ;; Remove erros de type checking comum em Django
        lsp-pyright-diagnostic-severity-overrides
        '(("reportUnknownMemberType" . "none")
          ("reportUnknownVariableType" . "none")
          ("reportUnknownArgumentType" . "none")
          ("reportUnknownParameterType" . "none")
          ("reportMissingTypeStubs" . "none")
          ("reportGeneralTypeIssues" . "none")
          ("reportOptionalMemberAccess" . "none")
          ("reportOptionalSubscript" . "none")
          ("reportPrivateImportUsage" . "none")
          ("reportAttributeAccessIssue" . "none")
          ("reportIncompatibleMethodOverride" . "none"))))

(after! flycheck
  (setq flycheck-checker-error-threshold nil
        flycheck-indication-mode 'left-fringe
        flycheck-highlighting-mode 'lines))

(after! lsp-mode
  (setq lsp-diagnostics-provider :none)
  
  (set-face-attribute 'lsp-face-highlight-textual nil :underline nil :background nil)
  (set-face-attribute 'lsp-face-highlight-read nil :underline nil :background nil)
  (set-face-attribute 'lsp-face-highlight-write nil :underline nil :background nil))
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
(map! :leader
      (:prefix ("c" . "code")
       :desc "Code actions"        "a" #'lsp-execute-code-action
       :desc "Rename symbol"       "r" #'lsp-rename
       :desc "Format buffer"       "f" #'apheleia-format-buffer
       :desc "Organize imports"    "o" #'lsp-organize-imports
       :desc "Hover doc"           "k" #'lsp-describe-thing-at-point)
      
      (:prefix ("g" . "goto")
       :desc "Definition"          "d" #'lsp-find-definition
       :desc "References"          "r" #'lsp-find-references
       :desc "Implementations"     "i" #'lsp-find-implementation
       :desc "Type definition"     "t" #'lsp-find-type-definition))
(use-package! ejc-sql
  :commands (ejc-sql-mode ejc-sql-connect)
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
(use-package! vterm
  :commands vterm
  :config
  (setq vterm-shell "/bin/zsh"))
