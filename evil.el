;;; evil.el -- Configuratons for Evil

;;; Commentary:
;;  This grew too large to maintain in main init file.

;;; Code:

(use-package evil
  :ensure evil
  :config
  (evil-mode 1)

  (add-to-list 'evil-emacs-state-modes 'git-rebase-mode)
  (add-to-list 'evil-emacs-state-modes 'flycheck-error-list-mode)
  (add-to-list 'evil-emacs-state-modes 'term-mode)
  (add-to-list 'evil-emacs-state-modes 'osx-dictionary-mode)

	(evil-add-hjkl-bindings osx-dictionary-mode-map 'emacs
    (kbd "0")       'evil-beginning-of-line
    (kbd "$")       'evil-end-of-line
    (kbd "/")       'evil-search-forward
    (kbd "b")       'evil-backward-word-begin
    (kbd "B")       'evil-backward-WORD-begin
    (kbd "E")       'evil-forward-WORD-end
    (kbd "e")       'evil-forward-word-end
    (kbd "g_")      'evil-last-non-blank
    (kbd "gg")      'evil-goto-first-line
    (kbd "G")       'evil-goto-last-line
    (kbd "n")       'evil-search-next
    (kbd "N")       'evil-search-previous
    (kbd "W")       'evil-forward-WORD-begin
    (kbd "w")       'evil-forward-word-begin
    (kbd "C-d")     'evil-scroll-down
    (kbd "C-u")     'evil-scroll-up)

  (setq evil-search-wrap t
        evil-regexp-search t)
  (setq evil-want-C-u-scroll t)
  (setq-default tab-width 2)

  (defvar my-leader-map (make-sparse-keymap)
    "Keymap for \"leader key\" shortcuts.")

  ;; binding "," to the keymap
  (define-key evil-normal-state-map "," my-leader-map)

  ;; change the "leader" key to space
  (define-key evil-normal-state-map "," 'evil-repeat-find-char-reverse)
  (define-key evil-normal-state-map (kbd "SPC") my-leader-map)

  ;; The default function bound to `m` go to middle of screen is not useful, so bind it to something useful (like macro execution).
  (define-key evil-normal-state-map "m" 'evil-execute-macro)

  (use-package evil-commentary
    :diminish
    :config
    (evil-commentary-mode))

  (use-package evil-magit
    :after evil
    :after magit
    :diminish
    :config
    (evil-define-key evil-magit-state magit-mode-map "?" 'evil-search-backward)) ; Magit already has `h` for help, so use `?` for its normal function

  (use-package evil-mc
    :diminish
    :config
    (global-evil-mc-mode 1))

  (use-package evil-rsi
    :diminish
    :config
    (evil-rsi-mode))

  (use-package evil-surround
    :ensure t
    :config
    (global-evil-surround-mode 1)))

(provide 'evil)
;;; evil.el ends here
