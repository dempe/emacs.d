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
(load-file "~/.emacs.d/package_configuration.el") ; Plugin related configs
(load-file "~/.emacs.d/helper-functions.el")      ; Load helper functions
(load-file "~/.emacs.d/journal-functions.el")     ; Load helper functions for journaling

;; (cld/load-spacemacs-dark-theme)

;; ~~~~~~~~~~~~~~~~~~~~~~~ KEYBINDINGS ~~~~~~~~~~~~~~~~~~~~~~~~~~~~

;; Many of the following keybindings are taken from the Spacemacs project.
;; They can be found here: https://github.com/syl20bnr/spacemacs/blob/c7a103a772d808101d7635ec10f292ab9202d9ee/layers/%2Bdistributions/spacemacs-base/keybindings.el
;; Information about keybinding with Emacs and Evil can be found here: https://github.com/noctuid/evil-guide

(define-key my-leader-map "!" 'shell-command)

;; avy ----------------------------------------------------------------------
(define-key my-leader-map "ac" 'avy-goto-char-2)
(define-key my-leader-map "al" 'avy-goto-line)

;; buffers ----------------------------------------------------------------------
(define-key my-leader-map "TAB" 'spacemacs/alternate-buffer)
(define-key my-leader-map "bN" 'spacemacs/new-empty-buffer)
(define-key my-leader-map "bY" 'spacemacs/paste-clipboard-to-whole-buffer)
(define-key my-leader-map "bb" 'helm-buffers-list)
(define-key my-leader-map "bd" 'kill-buffer)
(define-key my-leader-map "bn" 'next-buffer)
(define-key my-leader-map "bp" 'previous-buffer)
(define-key my-leader-map "br" (lambda () (interactive) (revert-buffer :ignore-auto :noconfirm)))
(define-key my-leader-map "bs" 'cld/switch-to-scratch-buffer)
(define-key my-leader-map "bw" 'read-only-mode)

;; These are commented out, because I never use them and wanted to use c for M-x
;; compilation ----------------------------------------------------------------
;;(define-key my-leader-map "cC" 'compile)
;;(define-key my-leader-map "ck" 'kill-compilation)
;;(define-key my-leader-map "cr" 'recompile)

;; dired ----------------------------------------------------------------------
(define-key my-leader-map "d" 'dired-mode)

;; errors ----------------------------------------------------------------------
(define-key my-leader-map "en" 'next-error)
(define-key my-leader-map "ep" 'previous-error)

;; files ----------------------------------------------------------------------
(define-key my-leader-map "fD" 'spacemacs/delete-current-buffer-file)
(define-key my-leader-map "fS" 'evil-write-all)
(define-key my-leader-map "fc" 'spacemacs/copy-file)
(define-key my-leader-map "fed" (lambda () (interactive) (find-file-existing "~/.emacs.d/init.el")))
(define-key my-leader-map "fr" (lambda () (interactive) (load-file "~/.emacs.d/init.el")))
;; (define-key my-leader-map "ff" 'find-file)
(define-key my-leader-map "ff" 'helm-find-files)
(define-key my-leader-map "fg" 'rgrep)
(define-key my-leader-map "fl" 'find-file-literally)
(define-key my-leader-map "fs" 'save-buffer)
(define-key my-leader-map "fvd" 'add-dir-local-variable)
(define-key my-leader-map "fvf" 'add-file-local-variable)
(define-key my-leader-map "fvp" 'add-file-local-variable-prop-line)
(define-key my-leader-map "fy" 'spacemacs/show-and-copy-buffer-filename)

;; evaluation  ------------------------------------------------------------------------
(define-key my-leader-map "eb" 'eval-buffer)

;; git ------------------------------------------------------------------------
(define-key my-leader-map "gs" 'magit-status)

;; help, helm -----------------------------------------------------------------------
(define-key my-leader-map "hdb" 'describe-bindings)
(define-key my-leader-map "hdc" 'describe-char)
(define-key my-leader-map "hdf" 'describe-function)
(define-key my-leader-map "hdk" 'describe-key)
(define-key my-leader-map "hdp" 'describe-package)
(define-key my-leader-map "hdt" 'describe-theme)
(define-key my-leader-map "hdv" 'describe-variable)
(define-key my-leader-map "hn" 'view-emacs-news)
(define-key my-leader-map "ha" 'helm-apropos)
(define-key my-leader-map "hkr" 'helm-show-kill-ring)
(define-key my-leader-map "hl" 'helm-locate)
(define-key my-leader-map "hpf" (lambda () (interactive) (helm-projectile-find-files)))
(define-key my-leader-map "ho" 'helm-occur)

;; insertion ---------------------------------------------------------------------
(define-key my-leader-map "ida" 'cld/insert-day)
(define-key my-leader-map "idl" 'cld/insert-date-long)
(define-key my-leader-map "idt" 'cld/insert-datetime)
(define-key my-leader-map "ij" 'spacemacs/evil-insert-line-below)
(define-key my-leader-map "ik" 'spacemacs/evil-insert-line-above)
(define-key my-leader-map "inl" 'cld/insert-line-below)
(define-key my-leader-map "inL" 'cld/insert-line-above)
(define-key my-leader-map "ik" 'spacemacs/evil-insert-line-above)
(define-key my-leader-map "is" 'cld/insert-space)
(define-key my-leader-map "it" 'cld/insert-time)

;; navigation, jumping, journal ---------------------------------------------------------
(define-key my-leader-map "j0" 'spacemacs/push-mark-and-goto-beginning-of-line)
(define-key my-leader-map "j$" 'spacemacs/push-mark-and-goto-end-of-line)
(define-key my-leader-map "jf" 'find-function)
(define-key my-leader-map "jo" 'open-line)                   ; insert newline above
(define-key my-leader-map "ji" 'cld/indent-region-or-buffer) ; note: spacemacs uses j=
(define-key my-leader-map "jl" 'cld/open-latest-journal-post)
(define-key my-leader-map "jp" 'cld/make-new-journal-post)
(define-key my-leader-map "js" 'cld/insert-sidenote)
(define-key my-leader-map "jv" 'find-variable)

;; narrowing ---------------------------------------------------------
(define-key my-leader-map "nf" 'narrow-to-defun)
(define-key my-leader-map "nos" 'org-narrow-to-subtree)
(define-key my-leader-map "np" 'narrow-to-page)
(define-key my-leader-map "nr" 'narrow-to-region)
(define-key my-leader-map "nw" 'widen)

;; org-mode ---------------------------------------------------------
(define-key my-leader-map "ob" 'org-backward-heading-same-level)
(define-key my-leader-map "ocb" 'cld/insert-org-code-block)
(define-key my-leader-map "oci" 'org-clock-in)
(define-key my-leader-map "odh" 'cld/org-demote-header)
(define-key my-leader-map "ods" 'org-demote-subtree)
(define-key my-leader-map "oes" 'org-edit-special)
(define-key my-leader-map "oem" 'cld/toggle-org-emphasis-markers)
(define-key my-leader-map "of" 'org-forward-heading-same-level)
(define-key my-leader-map "oha" 'outline-hide-body)                ;; "outline-hide-all"
(define-key my-leader-map "oic" 'cld/org-insert-checklist-item)
(define-key my-leader-map "oid" 'cld/org-insert-daily)
(define-key my-leader-map "oih" 'cld/org-insert-header)
(define-key my-leader-map "oil" 'org-insert-link)
(define-key my-leader-map "oIh" 'cld/org-insert-header-above)
(define-key my-leader-map "ons" 'cld/org-insert-new-subheader)
(define-key my-leader-map "oph" 'cld/org-promote-header)
(define-key my-leader-map "ops" 'org-promote-subtree)
(define-key my-leader-map "osa" 'outline-show-all)
(define-key my-leader-map "osp" 'org-set-property)
(define-key my-leader-map "ost" 'org-set-tags-command)
(define-key my-leader-map "ot" 'org-todo)

;; toggle, til, themes ---------------------------------------------------------
(define-key my-leader-map "til" 'cld/open-til)
(define-key my-leader-map "td" 'cld/load-spacemacs-dark-theme)
(define-key my-leader-map "tl" (lambda () (interactive) (load-theme 'spacemacs-light t)))
(define-key my-leader-map "tsl" (lambda () (interactive) (load-theme 'solarized-light t)))
(define-key my-leader-map "tsd" (lambda () (interactive) (load-theme 'solarized-dark t)))
(define-key my-leader-map "tn" 'cld/toggle-line-numbers)
(define-key my-leader-map "tw" 'whitespace-mode)

;; undo-tree ---------------------------------------------------------
(define-key my-leader-map "ut" 'undo-tree-visualize)

;; windows ---------------------------------------------------------
(define-key my-leader-map "w=" 'balance-windows)
(define-key my-leader-map "wF" 'make-frame)
(define-key my-leader-map "wH" 'evil-window-move-far-left)
(define-key my-leader-map "wf" 'follow-mode)
(define-key my-leader-map "wh" 'spacemacs/split-window-below-and-focus)  ; Spacemacs uses "ws" instead
(define-key my-leader-map "wk" 'delete-window)
(define-key my-leader-map "wo" 'other-frame)
(define-key my-leader-map "wv" 'spacemacs/split-window-right-and-focus)
(define-key my-leader-map "ww" 'other-window)

(define-key my-leader-map "xaa" 'align)
(define-key my-leader-map "xac" 'align-current)
(define-key my-leader-map "xc" 'count-region)
(define-key my-leader-map "xdw" 'delete-trailing-whitespace)
(define-key my-leader-map "xjc" 'set-justification-center)
(define-key my-leader-map "xjf" 'set-justification-full)
(define-key my-leader-map "xjl" 'set-justification-left)
(define-key my-leader-map "xjn" 'set-justification-none)
(define-key my-leader-map "xjr" 'set-justification-right)
(define-key my-leader-map "xtl" 'transpose-lines)
(define-key my-leader-map "xtw" 'transpose-words)
(define-key my-leader-map "xU" 'upcase-region)
(define-key my-leader-map "xu" 'downcase-region)

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
    (avy helm-projectile swiper-helm org-trello evil-magit spacemacs-theme spotlight osx-dictionary markdown-mode ido-completing-read+ desktop+ speed-type darkroom evil-surround evil-rsi evil-commentary solarized-theme diminish use-package evil-mc paradox rainbow-delimiters flycheck swiper magit helm which-key evil)))
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
