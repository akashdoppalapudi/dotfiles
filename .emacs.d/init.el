;; disable menu on startup
(menu-bar-mode -1)

;; disable tools on startup
(tool-bar-mode -1)

;; hide welcome screen
(setq inhibit-splash-screen t)

;; disable backup and lockfiles
(setq make-backup-files nil)
(setq create-lockfiles nil)

;; set font
(set-face-attribute 'default nil :font "FiraCode Nerd Font" :height 120)

;; enable line numbers and column number
(global-display-line-numbers-mode t)
(setq column-number-mode t)

;; set tab width to 4 spaces
(setq-default tab-width 4)

;; insert spaces instead of tabs
(setq-default indent-tabs-mode nil)

;; set word-wrap
(setq word-wrap t)
(add-hook 'text-mode-hook 'turn-on-visual-line-mode)

;; save command-history
(savehist-mode)

;; setup melpa
(require 'package)
(setq package-archives
      '(("melpa" . "https://melpa.org/packages/")
        ("gnu" . "https://elpa.gnu.org/packages/")
        ("nongnu" . "https://elpa.nongnu.org/nongnu/")))

;; activate packages
(package-initialize)

;; load theme
(use-package catppuccin-theme
  :ensure t
  :init
  (load-theme 'catppuccin :no-confirm))

(use-package solaire-mode
  :ensure t
  :init
  (solaire-global-mode 1))

;; All the icons
(use-package all-the-icons
  :ensure t)

;; Centaur Tabs
(use-package centaur-tabs
  :ensure t
  :config
  (centaur-tabs-mode t)
  (setq centaur-tabs-set-icons t)
  (setq centaur-tabs-plain-icons t)
  (setq centaur-tabs-set-modified-marker t)
  (setq centaur-tabs-height 32))

;; remap major modes with treesitter modes
(add-to-list 'major-mode-remap-alist '(rust-mode . rust-ts-mode))
(add-to-list 'major-mode-remap-alist '(c-mode . c-ts-mode))
(add-to-list 'major-mode-remap-alist '(c++-mode . c++-ts-mode))
(add-to-list 'major-mode-remap-alist '(yaml-mode . yaml-ts-mode))
(add-to-list 'major-mode-remap-alist '(python-mode . python-ts-mode))
(add-to-list 'major-mode-remap-alist '(go-mode . go-ts-mode))

;; Setup Eglot
(use-package eglot
  ;; no ensure is needed as eglot is built-in
  :init
  (setq eglot-events-buffer-size 0)
  :hook
  ((rust-ts-mode
    bash-ts-mode
    emacs-lisp-mode
    c-ts-mode
    c++-ts-mode
    cmake-ts-mode
    typescript-ts-mode
    tsx-ts-mode
    js-ts-mode
    dockerfile-ts-mode
    haskell-mode
    yaml-ts-mode
    python-ts-mode
    csharp-mode
    go-ts-mode
    html-mode) . eglot-ensure)
  (before-save-hook . eglot-format-buffer))

;; Setup Eldoc Box
(use-package eldoc-box
  :ensure t
  :hook
  (eglot-managed-mode . eldoc-box-hover-at-point-mode))

;; Company Mode
(use-package company
  :ensure t
  :hook
  (after-init . global-company-mode))

;; Setup Copilot
(use-package copilot
  :ensure t
  :hook
  (prog-mode . copilot-mode)
  :config
  ;; Set the keybinding for accepting copilot suggestions
  (define-key copilot-completion-map (kbd "<tab>") 'copilot-accept-completion)
  (define-key copilot-completion-map (kbd "TAB") 'copilot-accept-completion))

;; Treemacs Setup
(use-package treemacs
  :ensure t
  :bind
  (("s-T" . treemacs)
   ("s-C" . treemacs-add-and-display-current-project-exclusively))
  :config
  (setq treemacs-follow-mode t)
  (add-hook 'treemacs-mode-hook
            (lambda()
              (display-line-numbers-mode -1)
              (toggle-scroll-bar -1))))

;; Treemacs Nerd Icons
(use-package nerd-icons
  :ensure t)
(use-package treemacs-nerd-icons
  :ensure t
  :after treemacs
  :config
  (treemacs-load-theme "nerd-icons"))

;; Vterm Setup
(use-package vterm
  :ensure t
  :config
  (add-hook 'vterm-mode-hook
            (lambda()
              (display-line-numbers-mode -1)
              (toggle-scroll-bar -1))))

(use-package vterm-toggle
  :ensure t
  :bind
  (("<f2>" . vterm-toggle)))

;; Paredit Mode
(use-package paredit
  :ensure t
  :hook
  ((emacs-lisp-mode
    eval-expression-minibuffer-setup
    lisp-mode
    lisp-interaction-mode
    scheme-mode) . paredit-mode))

;; web development
(add-to-list 'auto-mode-alist '("\\.ts\\'" . typescript-ts-mode))
(add-to-list 'auto-mode-alist '("\\.tsx\\'" . tsx-ts-mode))
(add-to-list 'auto-mode-alist '("\\.js\\'" . js-ts-mode))
(add-to-list 'auto-mode-alist '("\\.jsx\\'" . tsx-ts-mode))

;; Emmet Mode
(use-package emmet-mode
  :ensure t
  :hook
  (html-mode . emmet-mode))

;; other file extension mode mapping
(add-to-list 'auto-mode-alist '("\\.json\\'" . json-ts-mode))
(add-to-list 'auto-mode-alist '("Dockerfile" . dockerfile-ts-mode))

;; CMakelists.txt activate cmake-ts-mode
(add-to-list 'auto-mode-alist '("CMakeLists.txt" . cmake-ts-mode))
(add-to-list 'auto-mode-alist '("\\.cmake\\'" . cmake-ts-mode))

;; Code action keybindings
(global-set-key (kbd "C-c C-k") 'comment-region)
(global-set-key (kbd "C-c C-u") 'uncomment-region)
(global-set-key (kbd "C-c C-h") 'eldoc)
(global-set-key (kbd "C-c C-d") 'xref-find-definitions)

;; transpose lines
(defun move-line-up ()
  (interactive)
  (transpose-lines 1)
  (previous-line 2))

(defun move-line-down ()
  (interactive)
  (next-line 1)
  (transpose-lines 1)
  (previous-line 1))

;; Keybindings for moving lines up/down
(global-set-key (kbd "M-<up>") 'move-line-up)
(global-set-key (kbd "M-<down>") 'move-line-down)

;; Setup Blamer
(use-package blamer
  :ensure t
  :config
  (global-blamer-mode 1)
  (set-face-attribute 'blamer-face nil
                      :inherit 'font-lock-comment-face
                      :italic t))

;; Org Agenda custmization
(use-package org
  ;; No ensure is needed as org is built-in
  :bind
  (("C-c a" . org-agenda))
  :config
  (setq org-directory "~/Documents/org")
  (setq org-agenda-files (list org-directory))
  (setq org-log-done t))

