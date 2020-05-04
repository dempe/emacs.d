;;; init.el -- Manual configs

;;; Commentary:
;; This file is loaded in init.el to keep my config separate from Emacs' auto-config.

;;; Code:

;; ~~~~~~~~~~~~~~~~~~~~~~~ GENERAL CONFIGURATION ~~~~~~~~~~~~~~~~~~~~~~~~~~~~

;; Open recent files
(recentf-mode 1)
(setq recentf-max-menu-items 25)
(setq recentf-max-saved-items 25)

(setq ispell-program-name "/usr/local/bin/ispell")
(setq initial-scratch-message "")
(setq truncate-lines nil)                      ; Turn on word wrap
(setq org-startup-truncated nil)               ; org-mode does not obey general word wrap setting
(setq inhibit-startup-screen t)
(setq show-paren-mode t)
(setq tool-bar-mode nil)
(setq menu-bar-mode nil)
(setq blink-cursor-mode nil)
(setq electric-pair-mode t)
(setq make-backup-files nil)

;; Setup tabs
(setq electric-indent-mode t)
(setq-default indent-tabs-mode nil)
(setq tab-width 2)

(put 'narrow-to-region 'disabled nil)

;; Set transparency
(set-frame-parameter (selected-frame) 'alpha '(100 50))
(add-to-list 'default-frame-alist '(alpha 100 50))

;; Make Dired stay in one buffer
(define-key dired-mode-map (kbd "RET") 'dired-find-alternate-file) ; was dired-advertised-find-file
(define-key dired-mode-map (kbd "^") (lambda () (interactive) (find-alternate-file "..")))  ; was dired-up-directory

(scroll-bar-mode -1)                              ; Hide scrollbars
(load-file "~/.emacs.d/customs.el")               ; Load automatically set custom values
(load-file "~/.emacs.d/package-configuration.el") ; Plugin related configs
(load-file "~/.emacs.d/helper-functions.el")      ; Load helper functions
(load-file "~/.emacs.d/journal-functions.el")     ; Load helper functions for journaling
(load-file "~/.emacs.d/keybindings.el")           ; Keybindings related configs

;; (cld/load-spacemacs-dark-theme)

(provide 'init.el)
;;; init.el ends here

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   (quote
    ("bffa9739ce0752a37d9b1eee78fc00ba159748f50dc328af4be661484848e476" default)))
 '(electric-pair-mode t)
 '(org-trello-current-prefix-keybinding "C-c o" nil (org-trello))
 '(package-selected-packages
   (quote
    (dired-narrow ace-jump-helm-line avy helm-projectile swiper-helm org-trello evil-magit spacemacs-theme spotlight osx-dictionary markdown-mode desktop+ speed-type darkroom evil-surround evil-rsi evil-commentary solarized-theme diminish use-package evil-mc paradox rainbow-delimiters flycheck swiper magit helm which-key evil)))
 '(safe-local-variable-values
   (quote
    ((org-todo-keyword-faces
      ("GOOD" . "green")
      ("OKAY" . "yellow")
      ("BAD" . "red"))))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(rainbow-delimiters-depth-1-face ((t (:inherit rainbow-delimiters-base-face :foreground "#ff0000"))))
 '(rainbow-delimiters-depth-2-face ((t (:inherit rainbow-delimiters-base-face :foreground "#ffa500"))))
 '(rainbow-delimiters-depth-3-face ((t (:inherit rainbow-delimiters-base-face :foreground "#ffff00"))))
 '(rainbow-delimiters-depth-4-face ((t (:inherit rainbow-delimiters-base-face :foreground "#00ff00"))))
 '(rainbow-delimiters-depth-5-face ((t (:inherit rainbow-delimiters-base-face :foreground "#0000ff"))))
 '(rainbow-delimiters-depth-6-face ((t (:inherit rainbow-delimiters-base-face :foreground "#ff00ff"))))
 '(rainbow-delimiters-depth-7-face ((t (:inherit rainbow-delimiters-base-face :foreground "#ffffff"))))
 '(rainbow-delimiters-depth-8-face ((t (:inherit rainbow-delimiters-base-face :foreground "#00ff00"))))
 '(rainbow-delimiters-depth-9-face ((t (:inherit rainbow-delimiters-base-face :foreground "#89cffa")))))
(put 'dired-find-alternate-file 'disabled nil)
