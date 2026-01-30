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
        lsp-pyright-type-checking-mode "off"
        lsp-pyright-diagnostic-mode "openFilesOnly"))

(after! lsp-mode
  ;; Desabilita TODOS os diagnósticos
  (setq lsp-diagnostics-provider :none
        
        ;; UI Elements
        lsp-headerline-breadcrumb-enable nil
        lsp-modeline-code-actions-enable nil
        lsp-modeline-diagnostics-enable nil
        lsp-modeline-workspace-status-enable nil
        lsp-signature-auto-activate nil
        lsp-lens-enable nil
        lsp-semantic-tokens-enable nil
        lsp-inlay-hint-enable nil
        
        ;; Performance
        lsp-idle-delay 0.5
        lsp-log-io nil
        lsp-enable-file-watchers nil))

(after! lsp-ui
  ;; Desabilita TUDO do lsp-ui
  (setq lsp-ui-sideline-enable nil
        lsp-ui-doc-enable nil
        lsp-ui-peek-enable t))

;; CRITICAL: Desabilita Flycheck completamente
(after! flycheck
  (setq flycheck-disabled-checkers '(lsp)
        flycheck-checker-error-threshold nil)
  (global-flycheck-mode -1))

;; Desabilita Flymake também
(after! flymake
  (setq flymake-diagnostic-functions nil)
  (remove-hook 'flymake-diagnostic-functions 'flymake-proc-legacy-flymake))

;; Remove underlines de todas as faces
(custom-set-faces!
  '(lsp-face-highlight-textual :underline nil :background nil)
  '(lsp-face-highlight-read :underline nil :background nil)
  '(lsp-face-highlight-write :underline nil :background nil)
  '(flycheck-error :underline nil)
  '(flycheck-warning :underline nil)
  '(flycheck-info :underline nil)
  '(flymake-error :underline nil)
  '(flymake-warning :underline nil)
  '(flymake-note :underline nil))
(use-package! apheleia
  :hook ((python-mode python-ts-mode) . apheleia-mode)
  :config
  (setf (alist-get 'python-mode apheleia-mode-alist) '(ruff-isort ruff))
  (setf (alist-get 'python-ts-mode apheleia-mode-alist) '(ruff-isort ruff))
  
  (setf (alist-get 'ruff apheleia-formatters)
        '("ruff" "format" "--stdin-filename" filepath))
  (setf (alist-get 'ruff-isort apheleia-formatters)
        '("ruff" "check" "--select" "I" "--fix" "--stdin-filename" filepath)))
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
