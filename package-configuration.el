;;; package_configuration.el -- Manual configs related to setting up various plugins

;;; Commentary:
;; This file is loaded in init.el.

;;; Code:

(setq package-archives '(("gnu" . "https://elpa.gnu.org/packages/")
                         ("melpa" . "https://melpa.org/packages/")
                         ("org" . "http://orgmode.org/elpa/")))
(require 'package)
(package-initialize)
(package-refresh-contents 1)                  ; Refresh packages asynchronously

;; Setup use-package
(eval-when-compile
  (require 'use-package))
(setq use-package-compute-statistics 1)       ; Generate a report of load times with M-x use-package-report

(load-file "~/.emacs.d/evil.el")              ; Load Evil configurations

(use-package avy
  :ensure t
  :config
  (setq
   avy-keys (number-sequence ?a ?z)           ; Any lower-case letter a-z.
   avy-background t))                         ; Grey-out background when searching

(use-package darkroom
  :ensure t
  :demand t
  :config
  ;; (setq darkroom-margins .25)
  (setq darkroom-text-scale-increase 1))

(use-package desktop+
  :diminish
  :config
  :demand t
  :config
  (desktop-save-mode 1))

(use-package diminish
  :config
  (diminish 'undo-tree-mode)
  (diminish 'eldoc-mode))

(use-package flycheck
  :diminish
  :config
  (global-flycheck-mode))

;; Bind <SPC c> to the M-x function (with Helm).
;; I'm using c, because I cannot get <SPC SPC> (what Spacemacs uses) to work.
(define-key my-leader-map "c" 'execute-extended-command)
(use-package helm
  :demand t
  :diminish
  :init
  (setq
   helm-always-two-windows               t
   helm-recentf-fuzzy-match              t
   helm-locate-fuzzy-match               nil ; locate fuzzy is worthless
   helm-M-x-fuzzy-match                  t
   helm-buffers-fuzzy-matching           t
   helm-semantic-fuzzy-match             t
   helm-apropos-fuzzy-match              t
   helm-imenu-fuzzy-match                t
   helm-lisp-fuzzy-completion            t
   helm-completion-in-region-fuzzy-match t
   helm-move-to-line-cycle-in-source     t ; move to end or beginning of source when reaching top or bottom of source.
   helm-ff-search-library-in-sexp        t ; search for library in `require' and `declare-function' sexp.
   helm-scroll-amount                    8 ; scroll 8 lines other window using M-<next>/M-<prior>
   helm-ff-file-name-history-use-recentf t
   helm-echo-input-in-header-line        t
   helm-split-window-default-side        'left)
  (helm-mode 1)

  :config

  ;; Adding these functions here until they become available in the main source in version 3.6.1
  (defun helm-toggle-visible-mark-forward ()
    (interactive)
    (helm-toggle-visible-mark 1))

  (defun helm-toggle-visible-mark-backward ()
    (interactive)
    (helm-toggle-visible-mark -1))

  ;; Remap keys.  Helm default mappings can be found here https://github.com/emacs-helm/helm/blob/0745fa347de4a2a83394dee8da99b092992b8d9b/helm.el
  (define-key helm-map (kbd "C-a")         #'helm-select-action)
  (define-key helm-map (kbd "C-n")         #'helm-next-page)
  (define-key helm-map (kbd "C-p")         #'helm-previous-page)
  (define-key helm-map (kbd "TAB")         #'helm-next-line)
  (define-key helm-map (kbd "<backtab>")   #'helm-previous-line)
  (define-key helm-map (kbd "<C-tab>")     #'helm-toggle-visible-mark-forward)
  (define-key helm-map (kbd "<C-S-tab>")   #'helm-toggle-visible-mark-backward)
  (define-key my-leader-map "c"            'helm-M-x))

(use-package ace-jump-helm-line
  :config
  (define-key helm-map (kbd "C-'") 'ace-jump-helm-line))

(use-package ivy
  :diminish)

;; Improves package menu
(use-package paradox
  :config
  (paradox-enable))

(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

(use-package solarized-theme
  :init
  ;; make the fringe stand out from the background
  (setq solarized-distinct-fringe-background t)

  ;; Don't change the font for some headings and titles
  (setq solarized-use-variable-pitch nil)

  ;; make the modeline high contrast
  (setq solarized-high-contrast-mode-line nil)

  ;; Use less bolding
  (setq solarized-use-less-bold t)

  ;; Use more italics
  (setq solarized-use-more-italic t)

  ;; Avoid all font-size changes
  (setq solarized-height-minus-1 1.0)
  (setq solarized-height-plus-1 1.0)
  (setq solarized-height-plus-2 1.0)
  (setq solarized-height-plus-3 1.0)
  (setq solarized-height-plus-4 1.0))

(use-package spacemacs-theme
  :defer t
  :init
  (setq spacemacs-theme-comment-bg nil)
  (setq spacemacs-theme-org-bold nil)
  (setq spacemacs-theme-org-height nil)
  (setq spacemacs-theme-org-highlight nil)
  (setq spacemacs-theme-org-priority-bold nil))

(use-package which-key
  :diminish
  :config
  (which-key-setup-side-window-right-bottom)
  (which-key-mode))

(provide 'package-configuration)
;;; package-configuration.el ends here
