(setq user-full-name "Lucas Cunegundes"
      user-mail-address "lucascsantana6@gmail.com")
(setq confirm-kill-emacs #'y-or-n-p
      default-directory "~")

(defun my/org-babel-tangle-config ()
  (when (string-equal (buffer-file-name)
                      (expand-file-name "~/dotfiles/doom/.config/doom/config.org"))
    (org-babel-tangle)))

(add-hook 'after-save-hook #'my/org-babel-tangle-config)

(setq browse-url-browser-function 'browse-url-generic)
(setq browse-url-generic-program "brave-browser")
(setq which-key-idle-delay 0.2)
(use-package! nerd-icons)

(setq
 doom-font (font-spec :family "JetBrainsMono Nerd Font" :size 15 :weight 'bold))

(custom-set-faces!
 '(italic :slant italic)
 '(bold :weight bold)
 '(bold-italic :weight bold :slant italic))

(setq doom-theme 'doom-tokyo-night
      display-line-numbers-type 'relative)

(beacon-mode 1)
(after! org
  (setq org-directory "~/Documentos/org"
        org-agenda-files '("~/Documentos/org/agenda.org")
        org-log-done 'time)
  (add-hook 'org-mode-hook #'org-bullets-mode))
;; Carregar .env do projeto automaticamente
(defun my/load-project-env ()
  "Load .env from current project root into environment variables."
  (let* ((root (or (projectile-project-root) default-directory))
         (env-file (expand-file-name ".env" root)))
    (when (file-exists-p env-file)
      (with-temp-buffer
        (insert-file-contents env-file)
        (goto-char (point-min))
        (while (re-search-forward "^\\([^=]+\\)=\\\"?\\([^\"\n]*\\)\\\"?$" nil t)
          (let ((key (match-string 1))
                (val (match-string 2)))
            (when (and key (string-match "^[A-Za-z_]+$" key))
              (setenv key val))))))))

;; Python mode hooks
(after! python
  (add-hook 'python-mode-hook #'pyvenv-mode)
  (add-hook 'python-mode-hook #'lsp-deferred)
  (add-hook 'python-mode-hook #'my/load-project-env))

;; LSP Pyright config - usar interpreter do .venv
(after! lsp-pyright
  (setq lsp-pyright-langserver-command "basedpyright-langserver"
        lsp-pyright-type-checking-mode "off"
        lsp-pyright-diagnostic-mode "openFilesOnly"

        ;; Supressões para Django
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

;; Flycheck
(after! flycheck
  (setq flycheck-indication-mode 'left-fringe)
  (setq flycheck-highlighting-mode nil)
  (setq flycheck-display-errors-function nil)
  (setq flycheck-idle-change-delay 0.5)
  (setq flycheck-checker-error-threshold nil)
  (setq flycheck-check-syntax-automatically '(save idle-change mode-enabled)))

;; Company
(after! company
  (setq company-idle-delay 0.1
        company-minimum-prefix-length 1))

;; Corfu desabilitado (usando company)
(after! corfu
  (global-corfu-mode 0))

;; LSP Mode
(after! lsp-mode
  (add-hook 'lsp-mode-hook (lambda () (lsp-browser-mode -1)))
  (setq lsp-diagnostics-provider :flycheck)
  (setq lsp-diagnostics-attributes
        '((unnecessary :foreground "gray")
          (deprecated :strike-through t)))
  (setq lsp-headerline-breadcrumb-enable t
        lsp-headerline-breadcrumb-icons-enable t
        lsp-headerline-breadcrumb-enable-symbol-numbers nil
        lsp-headerline-breadcrumb-enable-diagnostics nil)
  (setq lsp-icons-provider 'nerd-icons)
  (setq lsp-idle-delay 0.2
        lsp-log-io nil
        lsp-completion-provider :capf
        lsp-enable-file-watchers nil
        lsp-enable-folding t
        lsp-enable-text-document-color t
        lsp-enable-on-type-formatting nil
        lsp-enable-snippet nil
        lsp-enable-symbol-highlighting t
        lsp-modeline-diagnostics-enable nil
        lsp-modeline-code-actions-enable nil
        lsp-modeline-workspace-status-enable t
        lsp-semantic-tokens-enable nil
        lsp-completion-enable t
        lsp-completion-show-detail t
        lsp-completion-show-kind t
        lsp-enable-links nil))

;; LSP UI
(after! lsp-ui
  (add-hook 'lsp-mode-hook #'lsp-ui-mode)
  (setq lsp-ui-doc-enable t
        lsp-ui-doc-use-childframe t
        lsp-ui-doc-position 'at-point
        lsp-ui-doc-max-height 12
        lsp-ui-doc-max-width 80
        lsp-ui-doc-show-with-cursor t
        lsp-ui-doc-show-with-mouse nil
        lsp-ui-doc-delay 0.2
        lsp-ui-sideline-enable t
        lsp-ui-sideline-show-code-actions nil
        lsp-ui-sideline-show-diagnostics t
        lsp-ui-sideline-update-mode 'point
        lsp-ui-sideline-show-hover nil
        lsp-ui-sideline-delay 0.2
        lsp-ui-sideline-ignore-duplicate t
        lsp-ui-peek-enable t))
(after! dape
  (setq dape-buffer-window-arrangement 'right
        dape-info-hide-mode-line t)

  ;; Debug local no .venv do projeto
  (add-to-list
   'dape-configs
   `(python-local
     modes (python-mode python-ts-mode)
     command "python"
     command-args ("-m" "debugpy.adapter")
     port 5678
     host "127.0.0.1"
     cwd ,(lambda () (or (projectile-project-root) default-directory)))))
;; PGmacs setup - conecta usando variáveis do .env do projeto
(use-package pgmacs
  :after pg
  :commands (pgmacs pgmacs-open-string pgmacs-open-uri)
  :config
  (setq pgmacs-default-display-limit 100)
  (setq pgmacs-widget-use-proportional-font nil)

  ;; Função para conectar ao banco do projeto atual
  (defun pgmacs-connect-current-project ()
    "Connect to PostgreSQL using .env from current project."
    (interactive)
    (my/load-project-env)
    (let* ((user (or (getenv "POSTGRES_USER") "postgres"))
           (password (or (getenv "POSTGRES_PASSWORD") ""))
           (dbname (or (getenv "POSTGRES_NAME") "postgres"))
           (host (or (getenv "POSTGRES_HOST") "localhost"))
           (port (or (getenv "POSTGRES_PORT") "5432")))
      (pgmacs-open-string
       (format "user=%s password=%s dbname=%s host=%s port=%s"
               user password dbname host port))
      (message "Connected to %s at %s" dbname host)))

  ;; Funções de conveniência para projetos específicos
  (defun pgmacs-connect-nexus ()
    (interactive)
    (let ((projectile-known-projects-cache nil))
      (projectile-add-known-project "~/Code/nexus/"))
    (find-file "~/Code/nexus/.env")
    (my/load-project-env)
    (pgmacs-open-string
     "user=postgres password=jmnexus2023 dbname=nexus_rfid host=psql-nexus port=5432"))

  (defun pgmacs-connect-alianca ()
    (interactive)
    (let ((projectile-known-projects-cache nil))
      (projectile-add-known-project "~/Code/alianca/apps/backend/"))
    (find-file "~/Code/alianca/apps/backend/.env")
    (my/load-project-env)
    (pgmacs-open-string
     "user=postgres password=jmalianca2023 dbname=alianca_rfid host=0.0.0.0 port=5432")))

;; Funções utilitárias para SQL
(defun pg-list-tables ()
  "List tables in PostgreSQL database."
  (interactive)
  (pgmacs-connect-current-project)
  (pgmacs-open-string
   (format "SELECT table_name FROM information_schema.tables WHERE table_schema='public' ORDER BY table_name;")))

(defun pg-describe-table (table-name)
  "Describe TABLE-NAME structure."
  (interactive "sTable name: ")
  (pgmacs-connect-current-project)
  (pgmacs-open-string
   (format "SELECT column_name, data_type, is_nullable, column_default
FROM information_schema.columns
WHERE table_name = '%s'
ORDER BY ordinal_position;" table-name)))

;; Key bindings for SQL
(map! :leader
      (:prefix ("e" . "custom")
       (:prefix ("d" . "database")
        :desc "Connect to current project DB" "c" #'pgmacs-connect-current-project
        :desc "Connect to nexus" "n" #'pgmacs-connect-nexus
        :desc "Connect to alianca" "a" #'pgmacs-connect-alianca
        :desc "List tables" "t" #'pg-list-tables
        :desc "Describe table" "d" #'pg-describe-table)))
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
(setq vterm-environment '("TERM=xterm-256color"))
(set-language-environment "UTF-8")
(set-default-coding-systems 'utf-8)

(custom-set-faces!
 '(vterm :family "JetBrainsMono Nerd Font"))

(after! vterm
  (setq vterm-buffer-name-string "vterm %s")
  (setq vterm-shell "/bin/zsh")

  (defadvice! +vterm-use-current-directory-a (fn &rest args)
    "Make vterm open in the directory of the current buffer."
    :around #'vterm
    (let ((default-directory (or (and (buffer-file-name)
                                      (file-name-directory (buffer-file-name)))
                                 (and (eq major-mode 'dired-mode)
                                      (dired-current-directory))
                                 default-directory)))
      (apply fn args)))

  (defadvice! +vterm-use-current-directory-b (fn &rest args)
    "Make Doom's vterm commands open in the directory of the current buffer."
    :around #'+vterm/here
    (let ((default-directory (or (and (buffer-file-name)
                                      (file-name-directory (buffer-file-name)))
                                 (and (eq major-mode 'dired-mode)
                                      (dired-current-directory))
                                 default-directory)))
      (apply fn args))))
(after! magit
  (setq +magit-hub-features t))
(after! projectile
  (setq projectile-project-search-path '("~/Code/" "~/Projects/" "~/Projects/work/" "~/Projects/personal/" "~/notes/" "~/notes/org/")))
(require 'treemacs-all-the-icons)
(setq doom-themes-treemacs-theme "all-the-icons")
