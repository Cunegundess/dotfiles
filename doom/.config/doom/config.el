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
(setq doom-font (font-spec :family "JetBrains Mono Nerd Font" :size 13 :weight 'bold)
      doom-variable-pitch-font (font-spec :family "JetBrains Mono Nerd Font" :size 13))
;;
;; If you or Emacs can't find your font, use 'M-x describe-font' to look them
;; up, `M-x eval-region' to execute elisp code, and 'M-x doom/reload-font' to
;; refresh your font settings. If Emacs still can't find your font, it likely
;; wasn't installed correctly. Font issues are rarely Doom issues!

;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. This is the default:
(setq doom-theme 'doom-tokyo-night)

;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
(setq display-line-numbers-type 'relative)

;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!
(setq org-directory "~/org/")


;; Whenever you reconfigure a package, make sure to wrap your config in an
;; `after!' block, otherwise Doom's defaults may override your settings. E.g.
;;
;;   (after! PACKAGE
;;     (setq x y))
;;
;;

;; Org mode configuration with org-bullets
(use-package! org
  :config
  (setq org-directory "~/Documentos/org/")
  (setq org-agenda-files (list "~/Documentos/org/agenda.org"))
  (setq org-log-done 'time)

  ;; Enable org-bullets
  (add-hook 'org-mode-hook 'org-bullets-mode))

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

;; DAP Mode configuration - CONFIGURAÇÃO CORRIGIDA
(use-package! dap-mode
  :ensure nil  ; Já instalado via packages.el
  :after (lsp-mode python)
  :config
  ;; Carregar primeiro os módulos necessários
  (require 'dap-python)
  (require 'dap-hydra)

  ;; Inicializar DAP mode
  (dap-mode 1)
  (dap-ui-mode 1)
  (dap-tooltip-mode 1)
  (tooltip-mode 1)

  ;; Python-specific configuration
  (setq dap-python-debugger 'debugpy)
  (setq dap-python-executable "python3")

  ;; Helper function to get workspace directory
  (defun my/dap-get-workspace-root ()
    (or (if (buffer-file-name)
            (projectile-project-root)
          default-directory)
        default-directory))

  ;; Docker debug template
  (dap-register-debug-template
   "Python Docker"
   (list :type "python"
         :request "attach"
         :name "Python Docker"
         :host "localhost"
         :port 5678
         :pathMappings (vector (list :localRoot (my/dap-get-workspace-root)
                                    :remoteRoot "/app"))
         :justMyCode :json-false))

  ;; Keybindings DAP
  (map! :leader
        (:prefix ("d" . "debug")
         :desc "Start debug"       "s" #'dap-debug
         :desc "Next"              "n" #'dap-next
         :desc "Step in"           "i" #'dap-step-in
         :desc "Step out"          "o" #'dap-step-out
         :desc "Continue"          "c" #'dap-continue
         :desc "Toggle breakpoint" "b" #'dap-breakpoint-toggle
         :desc "Debug hydra"       "h" #'dap-hydra)))

;; Configuração do formatador ruff
(use-package! format-all
  :hook (python-mode . format-all-mode)
  :config
  (setq format-all-formatters
        '(("Python" (ruff-format))))

  ;; Função para formatar com ruff
  (defun my/python-format-buffer ()
    "Format current Python buffer with ruff"
    (interactive)
    (when (executable-find "ruff")
      (call-process-shell-command
       (format "ruff format --line-length 88 %s" (buffer-file-name))
       nil 0)))

  (add-hook 'before-save-hook 'my/python-format-buffer nil t))

;; Improved Docker debug function with error handling
(defun my/debug-docker ()
  "Connect to Docker debugpy with proper error handling"
  (interactive)
  (let ((config (list :type "python"
                      :request "attach"
                      :name "Docker Debug"
                      :host "localhost"
                      :port 5678
                      :pathMappings (vector (list :localRoot (my/dap-get-workspace-root)
                                                 :remoteRoot "/app"))
                      :justMyCode :json-false)))
    (condition-case err
        (dap-debug config)
      (error (message "Debug connection failed: %s" (error-message-string err))))))

;; Add keybinding for Docker debug
(map! :leader :desc "Docker debug" "d D" #'my/debug-docker)

;; Keybindings úteis para Python development
(map! :leader
      (:prefix ("c" . "code")
       :desc "Organize imports" "i" #'lsp-organize-imports
       :desc "Format buffer"    "f" #'my/python-format-buffer
       :desc "Ruff check"       "r" #'flycheck-checker))

;; Função para verificar se DAP está carregado corretamente
(defun my/check-dap-load ()
  "Check if DAP is properly loaded"
  (interactive)
  (if (featurep 'dap-mode)
      (message "✅ DAP mode is loaded")
    (message "❌ DAP mode is NOT loaded"))
  (if (fboundp 'dap-debug)
      (message "✅ dap-debug function is available")
    (message "❌ dap-debug function is NOT available")))

;; Configuração de fallback se DAP não carregar
(defun my/safe-dap-debug ()
  "Safe wrapper for dap-debug that handles loading issues"
  (interactive)
  (unless (featurep 'dap-mode)
    (require 'dap-mode)
    (require 'dap-python)
    (dap-mode 1))
  (call-interactively 'dap-debug))

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
