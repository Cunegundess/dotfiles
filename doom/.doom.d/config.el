;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

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

;; --------------------
;; ORG
;; --------------------
(use-package! org
  :config
  (setq org-directory "~/Documentos/"
        org-agenda-files '("~/Documentos/agenda.org")
        org-log-done 'time)
  (add-hook 'org-mode-hook #'org-bullets-mode))

;; ============================================================
;; PYTHON + EGLOT (LIMPO)
;; ============================================================

(after! python
  (setq python-shell-virtualenv-root ".venv")

  (add-hook 'python-mode-hook #'eglot-ensure)
  (add-hook 'python-ts-mode-hook #'eglot-ensure)

  (add-hook
   'python-mode-hook
   (lambda ()
     (when (projectile-project-root)
       (setq-local
        eglot-workspace-configuration
        `((:python
           (:analysis
            :typeCheckingMode "basic"
            :diagnosticMode "openFilesOnly"
            :autoImportCompletions t
            :autoSearchPaths t))
          (:basedpyright
           (:analysis
            :reportUnknownMemberType "none"
            :reportUnknownVariableType "none"
            :reportUnknownArgumentType "none"
            :inlayHints
            ;; (:variableTypes nil
            ;;  :functionReturnTypes nil
            ;;  :callArgumentNames nil
            ;;  :genericTypes nil)
            ))))))))

(after! eglot
  (setq eglot-inlay-hints-mode nil)
  (add-to-list 'eglot-server-programs
               '((python-mode python-ts-mode)
                 "basedpyright-langserver" "--stdio")))

;; ============================================================
;; FLYMAKE (FRINGE DIREITO, SEM INLINE)
;; ============================================================

(setq doom-diagnostic-buffer 'flymake)

(after! flymake
  (set-face-attribute 'flymake-warning nil :underline nil)

  (set-face-attribute 'flymake-error nil :underline t)

  (setq flymake-show-diagnostics-at-end-of-line nil
        flymake-indicator-type 'fringe
        flymake-fringe-indicator-position 'right-fringe
        flymake-no-changes-timeout 0.5))

;; ============================================================
;; KEYBINDS – DOOM NATIVO
;; ============================================================

(map! :leader
      (:prefix ("c" . "code")
       :desc "Code actions"        "a" #'eglot-code-actions
       :desc "Rename symbol"       "r" #'eglot-rename
       :desc "Definition"          "d" #'xref-find-definitions
       :desc "References"          "R" #'xref-find-references
       :desc "Hover doc"           "h" #'eldoc
       :desc "Next diagnostic"     "n" #'flymake-goto-next-error
       :desc "Prev diagnostic"     "p" #'flymake-goto-prev-error
       :desc "Buffer diagnostics"  "e" #'flymake-show-buffer-diagnostics
       :desc "Project diagnostics" "E" #'flymake-show-project-diagnostics))

;; ============================================================
;; FORMATAÇÃO – APHELEIA + RUFF
;; ============================================================

(use-package! apheleia
  :hook ((python-mode python-ts-mode) . apheleia-mode)
  :config
  (setf (alist-get 'python-mode apheleia-mode-alist) 'ruff)
  (setf (alist-get 'python-ts-mode apheleia-mode-alist) 'ruff)

  (setf (alist-get 'ruff apheleia-formatters)
        '("ruff" "format" "--stdin-filename" filepath))

  (map! :map python-mode-map
        :localleader
        "f" #'apheleia-format-buffer
        "F" #'apheleia-format-region))

;; ============================================================
;; DEBUG – DAPE (PRESERVADO)
;; ============================================================

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

;; ============================================================
;; DATABASE – EJC-SQL
;; ============================================================

(use-package! ejc-sql
  :commands ejc-sql-mode ejc-sql-connect
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

;; ============================================================
;; VTERM
;; ============================================================

(use-package! vterm
  :commands vterm
  :config
  (setq vterm-shell "/bin/zsh"))
