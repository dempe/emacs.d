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
  (setq avy-keys (number-sequence ?a ?z)))    ; Any lower-case letter a-z.

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
  ;; :bind (("C-a" . (lambda () (interactive) (helm-toggle-visible-marks)))) I could not get this to work.  I had to free up C-SPC so I could use this.

  :init
  (setq
   helm-always-two-windows t
   helm-apropos-fuzzy-match t
   helm-split-window-default-side 'left)

  :config
  (helm-mode 1)
  ;; (global-set-key (kbd "C-a") (lambda () (interactive) (helm-toggle-visible-marks)))
  (define-key my-leader-map "c" 'helm-M-x))

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
