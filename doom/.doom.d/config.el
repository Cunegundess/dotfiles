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

(setq doom-font (font-spec :family "Iosevka Nerd Font Mono" :size 15))

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

(after! lsp-pyright
  (setq lsp-pyright-langserver-command "basedpyright")
  (setq lsp-pyright-auto-import-completions t)
  (setq lsp-pyright-type-checking-mode "basic"))

;; DAP com localRoot dinâmico
(after! dap-mode
  (require 'dap-python)
  
  ;; Função para detectar o root do projeto atual
  (defun my/get-project-root ()
    "Retorna o root do projeto atual"
    (or (projectile-project-root)
        (when (featurep 'project)
          (cdr (project-current)))
        default-directory))
  
  ;; Template com localRoot dinâmico
  (dap-register-debug-template
   "Python Docker Attach"
   (list :type "python"
         :request "attach"
         :name "Python Docker Attach"
         :connect (list :host "127.0.0.1" :port 5678)
         ;; Função executada quando iniciar o debug
         :pathMappings (lambda ()
                        (vector 
                         (list :localRoot (my/get-project-root)
                               :remoteRoot "/app")))
         :justMyCode :json-false))
  
  ;; Ou template mais específico se seus projetos tiverem estrutura parecida
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

;; SQL com múltiplas conexões
(after! sql
  (setq sql-connection-alist
        '((postgres-local
           (sql-product 'postgres)
           (sql-server "localhost")
           (sql-user "seu_user")
           (sql-database "seu_db")
           (sql-port 5432))
          
          (postgres-docker
           (sql-product 'postgres)
           (sql-server "localhost")
           (sql-user "postgres")
           (sql-database "postgres")
           (sql-port 5433))))
  
  ;; Truncate long lines
  (add-hook 'sql-interactive-mode-hook
            (lambda ()
              (toggle-truncate-lines t)))
  
  ;; Keybindings
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
