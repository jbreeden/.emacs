;; This file should stand alone.

;; todo
;; - emmet
;; - org babel setup
;; - major-mode which-key
;; - apheleia
;; - hungry delete tweak

(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)

(unless (load "use-package" 'noerr)
  (package-refresh-contents)
  (package-install 'use-package))

(load "use-package" 'noerr)

;; --- Emacs options --------------------------------------------------------

(setq-default dired-listing-switches "-alh") ; Use human readable file sizes
(setq-default scroll-step 1)
(setq-default hscroll-step 1)
(setq-default completion-ignore-case t)
(setq-default confirm-kill-emacs (quote y-or-n-p))
(setq-default enable-recursive-minibuffers t)
(setq-default inhibit-startup-screen t)
(setq-default mouse-scroll-delay 0)
(setq-default mouse-wheel-scroll-amount '(1))
(setq-default ns-command-modifier (quote meta))
(setq-default read-buffer-completion-ignore-case t)
(setq-default read-file-name-completion-ignore-case t)
(setq-default repeat-on-final-keystroke t)
(setq-default truncate-lines t)
(setq-default vc-follow-symlinks t)
(setq-default wdired-allow-to-change-permissions t)
(setq-default indent-tabs-mode nil)
(setq-default auto-save-default nil) ; no littering
(setq-default make-backup-files nil) ; no littering
(setq-default require-final-newline t)

(progn ; Keep "custom" variables separate from this init file
  (setq-default custom-file "~/.emacs.d/custom.el")
  (when (file-exists-p custom-file)
    (load custom-file)))

;; Performance stuff, especially things suggested by
;; https://emacs-lsp.github.io/lsp-mode/page/performance/
(setq gc-cons-threshold (* 100 (expt 2 20))) ; 100MB
(setq read-process-output-max (expt 2 20)) ; 1MB

(put 'dired-find-alternate-file 'disabled nil)
(put 'narrow-to-region 'disabled nil)
(put 'scroll-left 'disabled nil)
(put 'scroll-right 'disabled nil)
(put 'upcase-region 'disabled nil)

;; --- Builtin Modes ------------------------------------------------

(tool-bar-mode -1)
(menu-bar-mode -1)
(savehist-mode 1)   ;; Save minibuffer history between sessions
(show-paren-mode 1)
(xterm-mouse-mode 1)
(global-display-line-numbers-mode 1)

;; --- Key bindings -------------------------------------------------

(bind-key* "M-o" 'other-window)
(bind-key* "C-c ." 'bury-buffer)
(bind-key* "C-c ," 'unbury-buffer)
(bind-key* "C-c b" 'switch-to-buffer)
(bind-key "C-c C-g" 'reload-major-mode)

;; --- Commands -----------------------------------------------------

(defun sudoedit (filename)
  (interactive "fsudoedit file: ")
  (message filename)
  (find-file (concat "/sudo::" (file-truename filename))))

(defun reload-major-mode ()
  (interactive)
  (funcall major-mode))

;; --- Shell stuff --------------------------------------------------

;; Make sure shell commands use my bashrc
(setenv "BASH_ENV" (concat (getenv "HOME") "/.bashrc"))

;; Setup default shell
(if (file-exists-p "/usr/local/bin/bash")
    (setq-default shell-file-name "/usr/local/bin/bash")
  (setq-default shell-file-name "bash"))

(setq-default explicit-shell-file-name shell-file-name)

;; ---- Packages --------------------------------------------

;; -- Builtins --

(use-package cc-vars
  :config
  (setq-default c-basic-offset 2))

(use-package hl-line
  :defer 2
  :config
  (global-hl-line-mode))

(use-package smerge-mode
  :defer t
  :config
  (bind-key (kbd "C-c C-c") 'smerge-keep-current smerge-mode-map))

(use-package recentf
  :config
  (recentf-mode))

(use-package js
  :init
  (setq-default js-jsx-syntax t))

;; -- External --

(use-package exec-path-from-shell
  :ensure t
  :config (exec-path-from-shell-initialize))

(use-package spacemacs-theme
  :defer t
  :ensure t)

(use-package solarized-theme
  :defer t
  :ensure t)

(use-package atom-one-dark-theme
  :defer t
  :ensure t)
(load-theme 'atom-one-dark 'noconfirm)

(use-package spaceline
  :ensure t
  :config
  (require 'spaceline-config)
  (spaceline-spacemacs-theme))

(use-package windmove ; Shift+Arrow moves point to adjacent windows
  :config
  (windmove-default-keybindings))

(use-package buffer-move ; Ctrl+Shift+Arrow moves buffer to adjacent windows
  :ensure t
  :bind (("<C-S-left>" . 'buf-move-left)
         ("<C-S-right>" . 'buf-move-right)
         ("<C-S-up>" . 'buf-move-up)
         ("<C-S-down>" . 'buf-move-down)))

(use-package which-key
  :ensure t
  :init
  (setq-default which-key-idle-delay 0.3)
  :config
  (which-key-mode))

(use-package selectrum
  :ensure t
  :config
  (selectrum-mode))

(use-package prescient
  :ensure t
  :config
  (prescient-persist-mode +1))

(use-package selectrum-prescient
  :ensure t
  :config
  (selectrum-prescient-mode +1))

(use-package ripgrep
  :ensure t)

(use-package deadgrep
  :ensure t
  :bind ("C-c /" . 'deadgrep))

(use-package ws-butler
  :ensure t
  :hook (prog-mode . ws-butler-mode))

(use-package hungry-delete
  :ensure t
  :config
  (setq-default hungry-delete-join-reluctantly t)
  (global-hungry-delete-mode))

(use-package move-text
  :ensure t
  :config
  (move-text-default-bindings))

(use-package string-inflection
  :defer 2
  :ensure t)

(use-package define-word
  :ensure t
  :commands (define-word define-word-at-point))

(use-package company
  :ensure t
  :bind (("C-c TAB" . company-complete))
  :config
  (global-company-mode)
  (setq-default company-async-timeout 6))

(use-package all-the-icons
  :if (display-graphic-p))

(use-package all-the-icons-completion
  :ensure t
  :if (display-graphic-p)
  :config (all-the-icons-completion-mode))

(use-package magit
  :defer 2
  :ensure t
  :bind (("C-c g s" . magit-status)
         ("C-c g f" . my-magit-find-file-in-worktree)
         ("C-c g g" . magit-file-dispatch))
  :config
  (progn
    (setq-default magit-display-buffer-function
                  'magit-display-buffer-same-window-except-diff-v1)

    (defun git ()
      (interactive)
      (call-interactively 'magit-git-command-topdir))

    (defun my-magit-find-file-in-worktree (file)
      (interactive (list
                    (magit-read-file-from-rev "HEAD" "Find file")))
      (find-file file))))

(use-package treemacs
  :ensure t
  :bind (("C-c t" . treemacs)))

(use-package projectile
  :ensure t
  :bind-keymap ("C-c p" . projectile-command-map)
  :init (setq-default projectile-switch-project-action 'projectile-dired))

(use-package yaml-mode
  :ensure t
  :mode "\\.ya\\?ml\\'")

(use-package go-mode
  :ensure t
  :mode ".go"
  :hook (go-mode . (lambda() (interactive) (setq-local tab-width 4))))

(use-package web-mode
  :ensure t
  :mode ("\\.\\([jt]sx?\\|html\\?\\|css\\|mustache\\)\\'" . web-mode)
  :hook ((web-mode . (lambda()
                       (when (or (equal web-mode-content-type "javascript")
                                 (equal web-mode-content-type "typescript"))
                         (setq web-mode-content-type "jsx")))))
  :config
  (setq-default web-mode-code-indent-offset 2)
  (setq-default web-mode-markup-indent-offset 2)
  (setq-default web-mode-css-indent-offset 2)
  (setq-default web-mode-sql-indent-offset 2))

(use-package eslint-fix
  :ensure t
  :commands (eslint-fix))

(use-package markdown-mode
  :ensure t
  :mode ".md")

(use-package terraform-mode
  :ensure t
  :mode ".tf")

(use-package pipenv
  :ensure t
  :defer 1)

(use-package editorconfig
  :ensure t
  :config
  (editorconfig-mode 1))

(use-package yasnippet
  :ensure t)

(use-package yasnippet-snippets
  :ensure t)

(use-package flycheck
  :ensure t
  :hook ((prog-mode . flycheck-mode))
  :bind-keymap (("C-c f" . flycheck-command-map)))

(use-package lsp-mode
  :ensure t
  :init
  (setq lsp-keymap-prefix "C-c l")
  :hook ((js-mode . lsp)
         (java-mode . lsp)
         (typescript-mode . lsp)
         (go-mode . lsp)
         (web-mode . lsp)
         (lsp-mode . lsp-enable-which-key-integration))
  :config
  (setq lsp-enable-indentation nil))

(use-package lsp-ui
  :ensure t
  :defer t)

(use-package lsp-java
  :ensure t
  :init
  (progn
    (setq lsp-java-format-settings-url "https://raw.githubusercontent.com/google/styleguide/gh-pages/eclipse-java-google-style.xml")
    (setq lsp-java-format-settings-profile "GoogleStyle")))
