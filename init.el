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

;; Setup tabs
(setq electric-indent-mode t)
(setq-default indent-tabs-mode nil)
(setq tab-width 2)

(put 'narrow-to-region 'disabled nil)

;; Set transparency
(set-frame-parameter (selected-frame) 'alpha '(100 50))
(add-to-list 'default-frame-alist '(alpha 100 50))

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
 '(compilation-message-face (quote default))
 '(cua-global-mark-cursor-color "#689d6a")
 '(cua-normal-cursor-color "#a89984")
 '(cua-overwrite-cursor-color "#d79921")
 '(cua-read-only-cursor-color "#98971a")
 '(custom-safe-themes
   (quote
    ("bffa9739ce0752a37d9b1eee78fc00ba159748f50dc328af4be661484848e476" default)))
 '(electric-pair-mode t)
 '(fci-rule-color "#32302f")
 '(highlight-changes-colors (quote ("#d3869b" "#b16286")))
 '(highlight-symbol-colors
   (quote
    ("#522a41fa2b3b" "#3821432637ec" "#5bbe348b2bf5" "#483d36c73def" "#43c0418329b9" "#538f36232679" "#317a3ddc3e5d")))
 '(highlight-symbol-foreground-color "#bdae93")
 '(highlight-tail-colors
   (quote
    (("#32302f" . 0)
     ("#747400" . 20)
     ("#2e7d33" . 30)
     ("#14676b" . 50)
     ("#a76e00" . 60)
     ("#a53600" . 70)
     ("#9f4d64" . 85)
     ("#32302f" . 100))))
 '(hl-bg-colors
   (quote
    ("#a76e00" "#a53600" "#b21b0a" "#9f4d64" "#8b2a58" "#14676b" "#2e7d33" "#747400")))
 '(hl-fg-colors
   (quote
    ("#282828" "#282828" "#282828" "#282828" "#282828" "#282828" "#282828" "#282828")))
 '(hl-paren-colors (quote ("#689d6a" "#d79921" "#458588" "#b16286" "#98971a")))
 '(nrepl-message-colors
   (quote
    ("#fb4933" "#d65d0e" "#d79921" "#747400" "#b9b340" "#14676b" "#689d6a" "#d3869b" "#b16286")))
 '(org-trello-current-prefix-keybinding "C-c o" nil (org-trello))
 '(package-selected-packages
   (quote
    (avy helm-projectile swiper-helm org-trello evil-magit spacemacs-theme spotlight osx-dictionary markdown-mode desktop+ speed-type darkroom evil-surround evil-rsi evil-commentary solarized-theme diminish use-package evil-mc paradox rainbow-delimiters flycheck swiper magit helm which-key evil)))
 '(pos-tip-background-color "#32302f")
 '(pos-tip-foreground-color "#bdae93")
 '(safe-local-variable-values
   (quote
    ((org-todo-keyword-faces
      ("GOOD" . "green")
      ("OKAY" . "yellow")
      ("BAD" . "red")))))
 '(smartrep-mode-line-active-bg (solarized-color-blend "#98971a" "#32302f" 0.2))
 '(term-default-bg-color "#282828")
 '(term-default-fg-color "#a89984")
 '(vc-annotate-background nil)
 '(vc-annotate-background-mode nil)
 '(vc-annotate-color-map
   (quote
    ((20 . "#fb4933")
     (40 . "#eb7b77d82bd3")
     (60 . "#e21e8997270c")
     (80 . "#d79921")
     (100 . "#c321997a1eab")
     (120 . "#b8ac99341d7b")
     (140 . "#ae1e98cb1c53")
     (160 . "#a37098411b32")
     (180 . "#98971a")
     (200 . "#8bd699a03aed")
     (220 . "#84849aa247bf")
     (240 . "#7c5b9ba153ba")
     (260 . "#731d9c9f5f38")
     (280 . "#689d6a")
     (300 . "#5cb793cf76ed")
     (320 . "#55e88efd7cec")
     (340 . "#4e348a3982c8")
     (360 . "#458588"))))
 '(vc-annotate-very-old-color nil)
 '(weechat-color-list
   (quote
    (unspecified "#282828" "#32302f" "#b21b0a" "#fb4933" "#747400" "#98971a" "#a76e00" "#d79921" "#14676b" "#458588" "#9f4d64" "#d3869b" "#2e7d33" "#689d6a" "#a89984" "#282828")))
 '(xterm-color-names
   ["#32302f" "#fb4933" "#98971a" "#d79921" "#458588" "#d3869b" "#689d6a" "#a89984"])
 '(xterm-color-names-bright
   ["#282828" "#d65d0e" "#7c6f64" "#282828" "#a89984" "#b16286" "#bdae93" "#fbf1c7"]))
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
