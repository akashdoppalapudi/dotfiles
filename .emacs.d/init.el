;; setup melpa
(require 'package)
(setq package-archives
      '(("melpa" . "https://melpa.org/packages/")
        ("gnu" . "https://elpa.gnu.org/packages/")
        ("nongnu" . "https://elpa.nongnu.org/nongnu/")))

;; Initialize packages
(package-initialize)
(unless package-archive-contents
  (package-refresh-contents))

;; custom functions

;; transpose lines
(defun move-line-up ()
  "Move up the current line."
  (interactive)
  (transpose-lines 1)
  (forward-line -2)
  (indent-according-to-mode))

(defun move-line-down ()
  "Move down the current line."
  (interactive)
  (forward-line 1)
  (transpose-lines 1)
  (forward-line -1)
  (indent-according-to-mode))

(defun my/toggle-eat-other-window ()
  "Show *eat* in the other window, or hide it if it's already visible."
  (interactive)
  (let* ((buf   (get-buffer-create "*eat*"))
         (win   (get-buffer-window buf)))
    (if win
        (delete-window win)
      (eat-other-window))))

;; Core emacs configuration

(use-package emacs
  :init
  (setq inhibit-startup-screen t) ; disable startup screen
  (setq make-backup-files nil) ; disable backup files
  (setq create-lockfiles nil) ; disable lock files
  (setq native-comp-async-report-warnings-errors nil) ; suppress warnings

  :config
  (tool-bar-mode -1) ; disable tool bar
  (menu-bar-mode -1) ; disable menu bar

  ;; Font settings
  (set-face-attribute 'default nil :font "FiraCode Nerd Font Mono" :height 120)

  ;; Editor behavior
  (setq-default tab-width 4)
  (setq-default indent-tabs-mode nil)
  (setq column-number-mode t)

  (global-display-line-numbers-mode t) ; enable line numbers

  :bind
  (("M-<up>" . move-line-up)
   ("M-<down>" . move-line-down)
   ("C-c C-k" . comment-region)
   ("C-c C-u" . uncomment-region)
   ("C-c C-h" . eldoc)
   ("C-c C-d" . xref-find-definitions)))

;; External package configuration

;; Load theme
(use-package catppuccin-theme
  :ensure t
  :config
  (load-theme 'catppuccin :no-confirm))

(use-package solaire-mode
  :ensure t
  :config
  (solaire-global-mode 1))

;; Git Blame
(use-package blamer
  :ensure t
  :after catppuccin-theme
  :config
  (set-face-attribute 'blamer-face nil
                      :inherit 'font-lock-comment-face
                      :italic t)
  (global-blamer-mode 1))

;; Save Command History
(use-package savehist
  :ensure t
  :config
  (savehist-mode 1))

;; Magit
(use-package magit
  :ensure t)

;; Centaur Tabs
(use-package centaur-tabs
  :ensure t
  :config
  (centaur-tabs-mode t)
  :custom
  (centaur-tabs-set-icons t)
  (centaur-tabs-plain-icons t)
  (centaur-tabs-set-modified-marker t)
  (centaur-tabs-height 32))

;; All the icons
(use-package all-the-icons
  :ensure t)

;; Treemacs
(use-package treemacs
  :ensure t
  :defer t
  :bind
  (("s-T" . treemacs)
   ("s-C" . treemacs-add-and-display-current-project-exclusively))
  :config
  (setq treemacs-follow-mode t)
  (add-hook 'treemacs-mode-hook
            (lambda ()
              (display-line-numbers-mode 0)
              (set-window-scroll-bars (selected-window) nil nil))))

;; Icons for Treemacs
(use-package treemacs-all-the-icons
  :ensure t
  :after treemacs
  :config
  (treemacs-load-theme "all-the-icons"))

;; Eat Setup
(use-package eat
  :ensure t
  :hook
  (eat-mode . (lambda ()
                (display-line-numbers-mode 0)
                (set-window-scroll-bars (selected-window) nil nil)))
  :bind
  (("C-`" . my/toggle-eat-other-window)))

;; Snippets
(use-package yasnippet
  :ensure t)

;; Company Mode
(use-package company
  :ensure t
  :hook
  (after-init . global-company-mode))

;; Few Major modes for languages
(use-package go-mode
  :ensure t)

(use-package rust-mode
  :ensure t)

;; Fountain mode for script writing
(use-package fountain-mode
  :ensure t)

;; other file extension mode mapping
(add-to-list 'auto-mode-alist '("\\.json\\'" . json-ts-mode))
(add-to-list 'auto-mode-alist '("Dockerfile" . dockerfile-ts-mode))

;; CMakelists.txt activate cmake-ts-mode
(add-to-list 'auto-mode-alist '("CMakeLists.txt" . cmake-ts-mode))
(add-to-list 'auto-mode-alist '("\\.cmake\\'" . cmake-ts-mode))

;; remap major modes with treesitter modes
(add-to-list 'major-mode-remap-alist '(rust-mode . rust-ts-mode))
(add-to-list 'major-mode-remap-alist '(c-mode . c-ts-mode))
(add-to-list 'major-mode-remap-alist '(c++-mode . c++-ts-mode))
(add-to-list 'major-mode-remap-alist '(yaml-mode . yaml-ts-mode))
(add-to-list 'major-mode-remap-alist '(python-mode . python-ts-mode))
(add-to-list 'major-mode-remap-alist '(go-mode . go-ts-mode))

;; Org Agenda customization
(use-package org
  ;; No ensure is needed as org is built-in
  :bind
  (("C-c a" . org-agenda))
  :config
  (setq org-directory "~/Documents/org")
  (setq org-agenda-files (list org-directory))
  (setq org-log-done t))

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

;; Setup Copilot
(use-package copilot
  :ensure t
  :hook
  (prog-mode . copilot-mode)
  :config
  ;; Set the keybinding for accepting copilot suggestions
  (define-key copilot-completion-map (kbd "<tab>") 'copilot-accept-completion)
  (define-key copilot-completion-map (kbd "TAB") 'copilot-accept-completion))


;; Prevent Emacs from writing customizations to this file.
;; All configuration should be managed with use-package.
(setq custom-file (make-temp-file "emacs-custom-"))
