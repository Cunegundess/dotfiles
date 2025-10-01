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

;; Configuração para Python com pyright + ruff
(after! python
  ;; Configurar pyright (LSP)
  (setq lsp-pyright-use-library-code-for-types t)
  (setq lsp-pyright-auto-import-completions t)

  ;; Configurar ruff (linter + formatter)
  (setq flycheck-python-ruff-executable "ruff")
  (add-hook 'python-mode-hook 'flycheck-mode))

;; Configuração do LSP mode para Python
(after! lsp-mode
  (setq lsp-pyright-multi-root nil)
  (setq lsp-enable-snippet nil)

  ;; Adicionar ruff como provider de formatção
  (add-to-list 'lsp-enabled-clients 'pyright))


(use-package dap-mode
  :after (lsp-mode python)
  :config
  ;; Carregar módulos necessários
  (require 'dap-python)
  (require 'dap-ui)
  (require 'dap-hydra)

  ;; Inicializar modos
  (dap-mode 1)
  (dap-ui-mode 1)
  (dap-tooltip-mode 1)
  (tooltip-mode 1)

  ;; Configuração do debugpy
  (setq dap-python-debugger 'debugpy)

  ;; Função para obter workspace root
  (defun my/dap-get-workspace-root ()
    (or (when (fboundp 'projectile-project-root)
          (projectile-project-root))
        default-directory))

  ;; Templates de debug
  (defun my/dap-python-docker-config ()
    "Python Docker configuration"
    (list :type "python"
          :request "attach"
          :name "Python Docker"
          :host "127.0.0.1"
          :port 5678
          :pathMappings (vector (list :localRoot (my/dap-get-workspace-root)
                                     :remoteRoot "/app"))
          :justMyCode :json-false))

  (defun my/dap-python-custom-config ()
    "Python Custom configuration"
    (list :type "python"
          :request "attach"
          :name "Python Custom"
          :host "127.0.0.1"
          :port 5678
          :pathMappings (vector (list :localRoot (lambda () (concat (my/dap-get-workspace-root) "/apps/backend"))
                                     :remoteRoot (lambda () (concat (my/dap-get-workspace-root) "/apps/backend"))))
          :cwd (lambda () (concat (my/dap-get-workspace-root) "/apps/backend"))
          :justMyCode :json-false))

  ;; Registrar templates (opcional - para uso com dap-debug)
  (dap-register-debug-template "Python Docker" (my/dap-python-docker-config))
  (dap-register-debug-template "Python Custom" (my/dap-python-custom-config))

  ;; Listeners para UI
  (add-hook 'dap-session-created-hook (lambda (_) (dap-ui-mode 1)))
  (add-hook 'dap-terminated-hook (lambda (_) (dap-ui-mode -1)))

  ;; Keybindings equivalentes ao Neovim
  (map! :leader
        (:prefix ("d" . "debug")
         "c" #'dap-continue
         "i" #'dap-step-in
         "n" #'dap-next
         "o" #'dap-step-out
         "b" #'dap-breakpoint-toggle
         "B" (lambda ()
               (interactive)
               (let ((condition (read-string "Breakpoint condition: ")))
                 (dap-breakpoint-condition condition)))
         "t" #'dap-ui-toggle
         "e" (lambda ()
               (interactive)
               (dap-ui-eval (thing-at-point 'symbol t)))
         "s" #'dap-debug
         "h" #'dap-hydra))

  ;; Funções específicas para cada tipo de debug
  (defun my/dap-docker-debug ()
    "Start Docker debug session"
    (interactive)
    (dap-debug (my/dap-python-docker-config)))

  (defun my/dap-custom-debug ()
    "Start Custom debug session"
    (interactive)
    (dap-debug (my/dap-python-custom-config)))

  (map! :leader "d D" #'my/dap-docker-debug
        :leader "d C" #'my/dap-custom-debug))

;; Configuração do dap-python
(use-package dap-python
  :after dap-mode
  :config
  (setq dap-python-executable "python3"))

;; Configuração do dap-ui
(use-package dap-ui
  :after dap-mode
  :config
  (setq dap-ui-controls-mode t
        dap-ui-sideline-mode t
        dap-ui-buffer-mode t
        dap-ui-sideline-show-hover t))

;; Função corrigida para selecionar configuração
(defun my/dap-select-configuration ()
  "Select debug configuration"
  (interactive)
  (let ((configs '(("Python Docker" . my/dap-python-docker-config)
                   ("Python Custom" . my/dap-python-custom-config))))
    (let ((selected (completing-read "Select debug configuration: "
                                    (mapcar 'car configs))))
      (let ((config-func (cdr (assoc selected configs))))
        (when config-func
          (dap-debug (funcall config-func)))))))

(map! :leader "d s" #'my/dap-select-configuration)

;; Comando para ajudar no setup
(defun my/dap-docker-help ()
  "Show Docker debugging help"
  (interactive)
  (my/dap-python-docker-setup))

(map! :leader "d H" #'my/dap-docker-help)

(map! :leader :desc "Comment line" "c ;" #'comment-line)

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
