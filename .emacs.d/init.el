;; disable menu on startup
(menu-bar-mode -1)

;; disable tools on startup
(tool-bar-mode -1)

;; Disable scroll bar on startup
(toggle-scroll-bar -1)

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

;; setup-melpa
(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(org-agenda-files (list org-directory))
 '(org-directory "~/Documents/org")
 '(package-selected-packages
   '(all-the-icons catppuccin-theme centaur-tabs company dpkg-dev-el
                   ebnf-mode eglot eldoc-box emmet-mode fountain-mode
                   go-mode haskell-mode js2-mode magit markdown-mode
                   paredit prettier protobuf-mode rust-mode
                   solaire-mode treemacs treemacs-magit
                   treemacs-nerd-icons vterm vterm-toggle yaml-mode
                   yasnippet)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

;; activate packages
(package-initialize)

;; --- Automatic Package Installation ---
;; Install packages listed in 'package-selected-packages' if they are missing.
;; This code runs *after* custom-set-variables has been evaluated.
(when (boundp 'package-selected-packages) ; Check if the variable is defined
  (dolist (pkg package-selected-packages) ; Iterate over the variable's value
    (unless (package-installed-p pkg)
      (message "Auto-installing missing package: %s" pkg) ; Informative message
      (package-install pkg))))

(require 'all-the-icons)

(require 'centaur-tabs)
(centaur-tabs-mode t)
(setq centaur-tabs-set-icons t)
(setq centaur-tabs-plain-icons t)
(setq centaur-tabs-set-modified-marker t)
(setq centaur-tabs-height 32)

;; remap major modes with treesitter modes
(add-to-list 'major-mode-remap-alist '(rust-mode . rust-ts-mode))
(add-to-list 'major-mode-remap-alist '(c-mode . c-ts-mode))
(add-to-list 'major-mode-remap-alist '(c++-mode . c++-ts-mode))
(add-to-list 'major-mode-remap-alist '(yaml-mode . yaml-ts-mode))
(add-to-list 'major-mode-remap-alist '(python-mode . python-ts-mode))
(add-to-list 'major-mode-remap-alist '(go-mode . go-ts-mode))

;; Turn off eglot logging
(setq eglot-events-buffer-size 0)

;; activate eglot
(add-hook 'rust-ts-mode-hook 'eglot-ensure)
(add-hook 'bash-ts-mode-hook 'eglot-ensure)
(add-hook 'emacs-lisp-mode-hook 'eglot-ensure)
(add-hook 'c-ts-mode-hook 'eglot-ensure)
(add-hook 'c++-ts-mode-hook 'eglot-ensure)
(add-hook 'cmake-ts-mode-hook 'eglot-ensure)
(add-hook 'typescript-ts-mode-hook 'eglot-ensure)
(add-hook 'tsx-ts-mode-hook 'eglot-ensure)
(add-hook 'js-ts-mode-hook 'eglot-ensure)
(add-hook 'dockerfile-ts-mode-hook 'eglot-ensure)
(add-hook 'haskell-mode-hook 'eglot-ensure)
(add-hook 'yaml-ts-mode-hook 'eglot-ensure)
(add-hook 'python-ts-mode-hook 'eglot-ensure)
(add-hook 'csharp-mode-hook 'eglot-ensure)
(add-hook 'go-ts-mode-hook 'eglot-ensure)
(add-hook 'html-mode-hook 'eglot-ensure)

;; auto format on save
(add-hook 'before-save-hook 'eglot-format-buffer)

;; company mode
(add-hook 'after-init-hook 'global-company-mode)

;; enable eldoc-box
(require 'eldoc-box)
(add-hook 'eglot-managed-mode-hook #'eldoc-box-hover-at-point-mode t)

;; treemacs setup
(global-set-key (kbd "s-T") 'treemacs)
(global-set-key (kbd "s-C") 'treemacs-add-and-display-current-project-exclusively)
(setq treemacs-follow-mode t)
(add-hook 'treemacs-mode-hook (lambda() (display-line-numbers-mode -1)))

;; nerd icons
(require 'nerd-icons)
(require 'treemacs-nerd-icons)
(treemacs-load-theme "nerd-icons")

;; vterm
(require 'vterm)
(add-hook 'vterm-mode-hook (lambda() (display-line-numbers-mode -1)))
;; vterm toggle
(global-set-key [f2] 'vterm-toggle)

;;paredit
(require 'paredit)
(autoload 'enable-paredit-mode "paredit" "Turn on psuedo-structural editing of lisp code" t)
(add-hook 'emacs-lisp-mode-hook                  #'enable-paredit-mode)
(add-hook 'eval-expression-minibuffer-setup-hook #'enable-paredit-mode)
(add-hook 'lisp-mode-hook                        #'enable-paredit-mode)
(add-hook 'lisp-interaction-mode-hook            #'enable-paredit-mode)
(add-hook 'scheme-mode-hook                      #'enable-paredit-mode)
(add-hook 'rust-ts-mode-hook                     #'enable-paredit-mode)
(add-hook 'go-ts-mode                            #'enable-paredit-mode)

;; web development
(add-to-list 'auto-mode-alist '("\\.ts\\'" . typescript-ts-mode))
(add-to-list 'auto-mode-alist '("\\.tsx\\'" . tsx-ts-mode))
(add-to-list 'auto-mode-alist '("\\.js\\'" . js-ts-mode))
(add-to-list 'auto-mode-alist '("\\.jsx\\'" . tsx-ts-mode))

(add-hook 'html-mode-hook 'emmet-mode)

;; other file extension mode mapping
(add-to-list 'auto-mode-alist '("\\.json\\'" . json-ts-mode))
(add-to-list 'auto-mode-alist '("Dockerfile" . dockerfile-ts-mode))

;; CMakelists.txt activate cmake-ts-mode
(add-to-list 'auto-mode-alist '("CMakeLists.txt" . cmake-ts-mode))

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

(global-set-key (kbd "M-<up>") 'move-line-up)
(global-set-key (kbd "M-<down>") 'move-line-down)


;; Org Agenda custmization
(global-set-key (kbd "C-c a") 'org-agenda)
(setq org-log-done t)

;; load theme
(load-theme 'catppuccin :no-confirm)
(solaire-global-mode t)

