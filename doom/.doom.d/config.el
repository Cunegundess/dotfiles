;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!


;; Some functionality uses this to identify you, e.g. GPG configuration, email
;; clients, file templates and snippets. It is optional.
(setq user-full-name "Lucas Cunegundes"
      user-mail-address "lucascsantana6@gmail.com")

;; Doom exposes five (optional) variables for controlling fonts in Doom:
;;
;; - `doom-font' -- the primary font to use
;; - `doom-variable-pitch-font' -- a non-monospace font (where applicable)
;; - `doom-big-font' -- used for `doom-big-font-mode'; use this for
;;   presentations or streaming.
;; - `doom-symbol-font' -- for symbols
;; - `doom-serif-font' -- for the `fixed-pitch-serif' face
;;
;; See 'C-h v doom-font' for documentation and more examples of what they
;; accept. For example:
;;

;; (setq doom-font (font-spec :family "JetBrains Mono Nerd Font" :size 13 :weight 'bold)
;;       doom-variable-pitch-font (font-spec :family "JetBrains Mono Nerd Font" :size 13))

(setq doom-font (font-spec :family "Iosevka Nerd Font Mono" :size 12))

(custom-set-faces!
  '(italic :slant italic)
  '(bold :weight bold)
  '(bold-italic :weight bold :slant italic))

;;
;; If you or Emacs can't find your font, use 'M-x describe-font' to look them
;; up, `M-x eval-region' to execute elisp code, and 'M-x doom/reload-font' to
;; refresh your font settings. If Emacs still can't find your font, it likely
;; wasn't installed correctly. Font issues are rarely Doom issues!

;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. This is the default:
(setq doom-theme 'doom-tokyo-night)
;; (setq doom-theme 'doom-meltbus)
;; (setq doom-theme 'doom-ayu-dark)

;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
(setq display-line-numbers-type 'relative)

(beacon-mode 1)

;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!
;; (setq org-directory "~/org/")
;; Org mode configuration with org-bullets
(use-package! org
  :config
  (setq org-directory "~/Documentos/")
  (setq org-agenda-files (list "~/Documentos/agenda.org"))
  (setq org-log-done 'time)

  ;; Enable org-bullets
  (add-hook 'org-mode-hook 'org-bullets-mode))

;; Whenever you reconfigure a package, make sure to wrap your config in an
;; `after!' block, otherwise Doom's defaults may override your settings. E.g.
;;
;;   (after! PACKAGE
;;     (setq x y))
;;
;;

(after! lsp-mode
  ;; Performance - importante para projetos grandes
  (setq lsp-idle-delay 0.5
        lsp-log-io nil
        lsp-enable-file-watchers nil
        lsp-file-watch-threshold 5000)
  
  ;; Desabilita recursos que podem conflitar com ruff
  (setq lsp-pylsp-plugins-flake8-enabled nil
        lsp-pylsp-plugins-pycodestyle-enabled nil
        lsp-pylsp-plugins-pylint-enabled nil
        lsp-pylsp-plugins-autopep8-enabled nil
        lsp-pylsp-plugins-yapf-enabled nil
        lsp-pylsp-plugins-mccabe-enabled nil
        lsp-pylsp-plugins-pyflakes-enabled nil))

(after! lsp-pyright
  ;; Usa basedpyright ao invés de pyright
  (setq lsp-pyright-langserver-command "basedpyright")
  
  ;; Configurações de type checking
  (setq lsp-pyright-type-checking-mode "standard")  ;; ou "basic", "strict"
  
  ;; Auto-import
  (setq lsp-pyright-auto-import-completions t
        lsp-pyright-auto-search-paths t)
  
  ;; Diagnóstico em tempo real
  (setq lsp-pyright-diagnostic-mode "workspace")
  
  ;; Desabilita organize imports (vamos usar ruff pra isso)
  (setq lsp-pyright-disable-organize-imports t)
  
  ;; Configurações adicionais para melhor experiência
  (setq lsp-pyright-use-library-code-for-types t
        lsp-pyright-venv-path ".")  ;; procura .venv no diretório do projeto
  
  ;; Inlay hints (opcional - mostra tipos inferidos)
  (setq lsp-pyright-basedpyright-inlay-hints-variable-types t
        lsp-pyright-basedpyright-inlay-hints-function-return-types t))

(after! python
  ;; Detecção automática de virtualenv (.venv nos containers)
  (setq python-shell-virtualenv-root ".venv")
  
  ;; Importar automaticamente módulos do projeto
  (add-hook 'python-mode-hook
            (lambda ()
              (when (projectile-project-root)
                (setq-local lsp-pyright-extra-paths
                           (vector (expand-file-name "." (projectile-project-root))))))))

(use-package! apheleia
  :hook (python-mode . apheleia-mode)
  :config
  ;; Configura ruff como formatador
  (setf (alist-get 'python-mode apheleia-mode-alist) 'ruff)
  (setf (alist-get 'python-ts-mode apheleia-mode-alist) 'ruff)
  
  ;; Comando do ruff (formata E organiza imports)
  (setf (alist-get 'ruff apheleia-formatters)
        '("ruff" "format" "--stdin-filename" filepath))
  
  ;; IMPORTANTE: Formatação manual, não automática
  (setq apheleia-mode nil)  ;; desabilita formatação ao salvar
  
  ;; Keybindings para formatar manualmente
  (map! :map python-mode-map
        :localleader
        :desc "Format buffer" "f" #'apheleia-format-buffer
        :desc "Format region" "F" #'apheleia-format-region))

;; Adiciona checker do ruff ao flycheck
(use-package! flycheck
  :config
  (flycheck-define-checker python-ruff
    "A Python syntax and style checker using Ruff."
    :command ("ruff" "check"
              "--output-format=text"
              "--stdin-filename" source-original
              "-")
    :standard-input t
    :error-filter (lambda (errors)
                    (flycheck-sanitize-errors
                     (flycheck-increment-error-columns errors)))
    :error-patterns
    ((error line-start
            (file-name) ":" line ":" column ": "
            (id (one-or-more (not (any ":")))) " "
            (message (one-or-more not-newline))
            line-end))
    :modes (python-mode python-ts-mode))
  
  ;; Adiciona ruff após o LSP checker
  (add-hook 'lsp-managed-mode-hook
            (lambda ()
              (when (derived-mode-p 'python-mode 'python-ts-mode)
                (flycheck-add-next-checker 'lsp 'python-ruff)))))

(after! dap-mode
  (require 'dap-python)
  
  (defun my/get-project-root ()
    "Retorna o root do projeto atual"
    (or (projectile-project-root)
        (when (featurep 'project)
          (cdr (project-current)))
        default-directory))
  
  (dap-register-debug-template
   "Python Docker Attach"
   (list :type "python"
         :request "attach"
         :name "Python Docker Attach"
         :connect (list :host "127.0.0.1" :port 5678)
         :pathMappings (lambda ()
                        (vector 
                         (list :localRoot (my/get-project-root)
                               :remoteRoot "/app")))
         :justMyCode :json-false))
  
  (dap-register-debug-template
   "Python Docker Django"
   (list :type "python"
         :request "attach"
         :name "Python Docker"
         :connect (list :host "127.0.0.1" :port 5678)
         :pathMappings (lambda ()
                        (let ((root (my/get-project-root)))
                          (vector 
                           (list :localRoot (expand-file-name "apps" root)
                                 :remoteRoot "/app/apps"))))
         :justMyCode :json-false)))

(use-package vterm
  :commands vterm
  :config
  (setq vterm-shell "/bin/zsh"))

(after! sql
  (setq sql-connection-alist
        '((postgres-local
           (sql-product 'postgres)
           (sql-server "localhost")
           (sql-user "postgres")
           (sql-database "alianca_rfid")
           (sql-port 5432))
          
          (postgres-docker
           (sql-product 'postgres)
           (sql-server "localhost")
           (sql-user "postgres")
           (sql-database "nexus_rfid")
           (sql-port 5433))))
  
  (add-hook 'sql-interactive-mode-hook
            (lambda ()
              (toggle-truncate-lines t)))
  
  (map! :map sql-mode-map
        :localleader
        "c" #'sql-connect
        "s" #'sql-send-paragraph
        "b" #'sql-send-buffer
        "r" #'sql-send-region))

;; The exceptions to this rule:
;;
;;   - Setting file/directory variables (like `org-directory')
;;   - Setting variables which explicitly tell you to set them before their
;;     package is loaded (see 'C-h v VARIABLE' to look up their documentation).
;;   - Setting doom variables (which start with 'doom-' or '+').
;;
;; Here are some additional functions/macros that will help you configure Doom.
;;
;; - `load!' for loading external *.el files relative to this one
;; - `use-package!' for configuring packages
;; - `after!' for running code after a package has loaded
;; - `add-load-path!' for adding directories to the `load-path', relative to
;;   this file. Emacs searches the `load-path' when you load packages with
;;   `require' or `use-package'.
;; - `map!' for binding new keys
;;
;; To get information about any of these functions/macros, move the cursor over
;; the highlighted symbol at press 'K' (non-evil users must press 'C-c c k').
;; This will open documentation for it, including demos of how they are used.
;; Alternatively, use `C-h o' to look up a symbol (functions, variables, faces,
;; etc).
;;
;; You can also try 'gd' (or 'C-c c d') to jump to their definition and see how
;; they are implemented.
