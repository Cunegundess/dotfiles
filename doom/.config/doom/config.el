(setq user-full-name "Lucas Cunegundes"
      user-mail-address "lucascsantana6@gmail.com")
(setq confirm-kill-emacs #'y-or-n-p
      default-directory "~")

(defun my/org-babel-tangle-config ()
  (when (string-equal (buffer-file-name)
                      (expand-file-name "~/dotfiles/doom/.config/doom/config.org"))
    (org-babel-tangle)))

(add-hook 'after-save-hook #'my/org-babel-tangle-config)

(setq browse-url-browser-function 'browse-url-generic
      browse-url-generic-program "brave-browser"
      which-key-idle-delay 0.2)
(use-package! nerd-icons)

(setq doom-font (font-spec :family "JetBrains Mono Nerd Font" :size 13 :weight 'bold)
      doom-unicode-font (font-spec :family "Noto Color Emoji")
      doom-emoji-font (font-spec :family "Noto Color Emoji"))

(custom-set-faces!
 '(italic :slant italic)
 '(bold :weight bold)
 '(bold-italic :weight bold :slant italic))

(setq doom-theme 'modus-vivendi)

(setq display-line-numbers-type 'relative)

;: accented borderless padded
(setq modus-themes-mode-line '(borderless)) 
(setq modus-themes-region '(accented))
;(setq modus-themes-region '(bg-only))
;(setq modus-themes-region '(bg-only no-extend))
(setq modus-themes-bold-constructs t)
;(setq modus-themes-italic-constructs t)
;(setq modus-themes-paren-match '(bold intense underline))
;(setq modus-themes-syntax '(faint))
;(setq modus-themes-syntax '(alt-syntax))
;(setq modus-themes-syntax '(green-strings yellow-comments))

(beacon-mode 1)
(load! "secrets")

(after! org
  (setq org-directory "~/Documentos/notes"
        org-agenda-files '("~/Documentos/notes/agenda.org" "~/Documentos/notes/gcal.org")
        org-log-done 'time)

  (add-hook 'org-mode-hook #'org-bullets-mode))

(after! org-gcal
  (setq org-gcal-client-id my/org-gcal-client-id
        org-gcal-client-secret my/org-gcal-client-secret
        org-gcal-fetch-file-alist
        '(("lucascsantana6@gmail.com"
           . "~/Documentos/notes/gcal.org"))))
(after! devdocs
  (setq devdocs-data-dir "~/.local/share/devdocs"))
;; Detectar root corretamente (suporte apps/backend)
(defun my/project-root ()
  (let ((root (projectile-project-root)))
    (if (and root (file-directory-p (expand-file-name "apps/backend" root)))
        (expand-file-name "apps/backend" root)
      root)))

;; Auto ativar .venv
(defun my/auto-activate-venv ()
  (let* ((root (or (my/project-root) default-directory))
         (venv (expand-file-name ".venv" root)))
    (when (file-directory-p venv)
      (pyvenv-activate venv)
      (message "Activated venv: %s" venv))))

;; Carregar .env corretamente
(defun my/load-project-env ()
  (let* ((root (my/project-root))
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
(after! company
  (global-company-mode -1))

(setq company-idle-delay nil
      company-minimum-prefix-length nil)

(use-package! corfu
  :init (global-corfu-mode)
  :custom
  (corfu-auto t)
  (corfu-auto-delay 0.1)
  (corfu-auto-prefix 1)
  (corfu-cycle t)
  (corfu-preview-current nil))

(use-package! cape
  :after corfu
  :init
  (add-to-list 'completion-at-point-functions #'cape-file)
  (add-to-list 'completion-at-point-functions #'cape-dabbrev)
  (add-to-list 'completion-at-point-functions #'cape-keyword))

(use-package! kind-icon
  :after corfu
  :init
  (add-hook 'corfu-mode-hook
            (lambda ()
              (require 'kind-icon)
              (add-to-list 'corfu-margin-formatters
                           #'kind-icon-margin-formatter)))
  :custom
  (kind-icon-default-face 'corfu-default))
(after! python
  (add-hook 'python-mode-hook #'my/auto-activate-venv)
  (add-hook 'python-mode-hook #'my/load-project-env))
  ;; (add-hook 'python-mode-hook #'lsp-deferred)
  ;; (add-hook 'python-mode-local-vars-hook #'lsp-deferred))
(after! lsp-pyright
  (setq lsp-pyright-langserver-command "basedpyright-langserver"
        lsp-pyright-type-checking-mode "off"
        lsp-pyright-diagnostic-mode "openFilesOnly"
        lsp-pyright-venv-path "."
        lsp-pyright-venv-directory ".venv"))
(after! lsp-mode
  (add-hook 'lsp-mode-hook #'lsp-ui-mode)
  (setq lsp-completion-provider :capf
        lsp-diagnostics-provider :flycheck
        lsp-headerline-breadcrumb-enable t
        lsp-headerline-breadcrumb-icons-enable t
        lsp-headerline-breadcrumb-enable-diagnostics nil
        lsp-enable-file-watchers nil
        lsp-icons-provider 'nerd-icons))

(use-package! lsp-ui
  :hook (lsp-mode . lsp-ui-mode)
  :config
  (setq lsp-ui-doc-enable t
        lsp-ui-doc-delay 0.2
        lsp-ui-doc-show-with-cursor t
        lsp-ui-sideline-enable t))
(after! dape
  (setq dape-buffer-window-arrangement 'right
        dape-info-hide-mode-line t)

  (add-to-list
   'dape-configs
   `(django
     modes (python-mode python-ts-mode)
     type "python"
     request "attach"
     connect (:host "127.0.0.1" :port 5678)
     cwd ,(lambda () (projectile-project-root))))

  (add-to-list
   'dape-configs
   `(alianca
     modes (python-mode python-ts-mode)
     type "python"
     request "attach"
     connect (:host "127.0.0.1" :port 5678)
     cwd ,(lambda () (my/project-root)))))
(use-package pgmacs
  :after pg
  :commands (pgmacs pgmacs-open-string pgmacs-open-uri)
  :config
  (setq pgmacs-default-display-limit 100)

  (defun pgmacs-connect-current-project ()
    (interactive)
    (my/load-project-env)
    (let* ((user (or (getenv "POSTGRES_USER") "postgres"))
           (password (or (getenv "POSTGRES_PASSWORD") ""))
           (dbname (or (getenv "POSTGRES_NAME") "postgres"))
           (host (or (getenv "POSTGRES_HOST") "localhost"))
           (port (or (getenv "POSTGRES_PORT") "5432")))
      (pgmacs-open-string
       (format "user=%s password=%s dbname=%s host=%s port=%s"
               user password dbname host port)))))
(map! :leader
      (:prefix ("c" . "code")
       :desc "Code actions" "a" #'lsp-execute-code-action
       :desc "Rename" "r" #'lsp-rename
       :desc "Format" "f" #'apheleia-format-buffer
       :desc "Imports" "o" #'lsp-organize-imports
       :desc "Hover" "k" #'lsp-describe-thing-at-point)

      (:prefix ("g" . "goto")
       :desc "Definition" "d" #'lsp-find-definition
       :desc "Refs" "r" #'lsp-find-references))
(after! vterm
  (setq vterm-shell "/bin/zsh"))
(after! projectile
  (setq projectile-project-search-path
        '("~/Code/" "~/Projects/" "~/Projects/work/")))
