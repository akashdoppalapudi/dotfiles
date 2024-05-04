;; disable menu on startup
(menu-bar-mode -1)

;; disable tools on startup
(tool-bar-mode -1)

;; disable scroll bar on startup
(toggle-scroll-bar -1)

;; hide welcome screen
(setq inhibit-splash-screen t)

;; disable backup and lockfiles
(setq make-backup-files nil)
(setq create-lockfiles nil)

;; set font
(set-face-attribute 'default nil :font "-JB-JetBrainsMono Nerd Font Mono-normal-normal-normal-*-*-*-*-*-m-0-iso10646-1" :height 120)

;; enable line numbers and column number
(global-display-line-numbers-mode t)
(setq column-number-mode t)

;; set tab width to 4 spaces
(setq-default tab-width 4)

;; insert spaces instead of tabs
(setq-default indent-tabs-mode nil)

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
 '(package-selected-packages
   '(company-web yasnippet prettier markdown-mode js2-mode web-mode emmet-mode lsp-mode rust-mode treemacs-magit magit eglot catppuccin-theme centaur-tabs paredit company vterm vterm-toggle treemacs treemacs-nerd-icons)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

;; activate packages
(package-initialize)

;; remap major modes with treesitter modes
(add-to-list 'major-mode-remap-alist '(rust-mode . rust-ts-mode))

;; activate eglot
(add-hook 'rust-ts-mode-hook 'eglot-ensure)
(add-hook 'bash-ts-mode-hook 'eglot-ensure)
(add-hook 'emacs-lisp-mode-hook 'eglot-ensure)

;; activate lsp
(require 'lsp-mode)
(add-hook 'web-mode-hook #'lsp)

;; auto format on save
(add-hook 'before-save-hook 'eglot-format-buffer)

;; company mode
(add-hook 'after-init-hook 'global-company-mode)

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

;; web development
(add-to-list 'auto-mode-alist '("\\.ts\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.css?\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.js\\'" . web-mode))

(defun my-web-mode-hook ()
  (set (make-local-variable 'company-backends) '(company-css company-web-html company-yasnippet company-files))
)
(add-hook 'web-mode-hook 'my-web-mode-hook)
(add-hook 'web-mode-hook 'emmet-mode)

(add-hook 'web-mode-before-auto-complete-hooks
          '(lambda ()
             (let ((web-mode-cur-language
  	                (web-mode-language-at-pos)))
               (if (string= web-mode-cur-language "php")
    	           (yas-activate-extra-mode 'php-mode)
      	         (yas-deactivate-extra-mode 'php-mode))
               (if (string= web-mode-cur-language "css")
    	           (setq emmet-use-css-transform t)
      	         (setq emmet-use-css-transform nil)))))

;; load theme
(load-theme 'catppuccin :no-confirm)

