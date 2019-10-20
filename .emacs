;; Zanders emacs config

;; ========== package stuff ==========

;; use package manager
(require 'package)

;; add add-ons repos
(add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages/"))
(add-to-list 'package-archives '("org" . "http://orgmode.org/elpa/"))
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))
(add-to-list 'package-archives '("melpa-stable" . "http://stable.melpa.org/packages/"))

;; initialize packages
(package-initialize)

;; make sure that use-package is installed
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(eval-when-compile
  (require 'use-package))


; enable rainbow delimiters
; (require 'rainbow-delimiters)
; use in all modes
; (add-hook 'prog-mode-hook 'rainbow-delimiters-mode)
; use in programmming related modes?
; (add-hook 'prog-mode-hook 'rainbow-delimiters-mode)

;; ========== choose to be evil ==========

(use-package evil
  :ensure t
  :config
  (evil-mode t)
  )


;; =========== theme ==========

;; the doom pack has many nice themes
(use-package doom-themes
  :ensure t
  :config
  (setq doom-themes-enable-bold t
    doom-themes-enable-italic t)
  ;(load-theme 'doom-one t)
  (load-theme 'doom-dracula t)
  (doom-themes-visual-bell-config)
  )


;; =========== ui stuff ==========

;; hide unnessecary ui elements
(scroll-bar-mode -1)
(tool-bar-mode -1)
(menu-bar-mode -1)

;; highlight current line
(global-hl-line-mode +1)
;; enable line numbers in the margin
(line-number-mode +1)
;; enable line numbers in the modeline
(global-display-line-numbers-mode 1)
;; enable column number in the modeline
(column-number-mode t)
;; shows the size of the buffer in the modeline
(size-indication-mode t)

;; disable startup screen and open with scratch screen instead
(setq inhibit-startup-screen t)

;; show path of file in model-line
(setq frame-title-format
      '((:eval (if (buffer-file-name)
       (abbreviate-file-name (buffer-file-name))
       "%b"))))

;; powerline mode-line theme
;(use-package smart-mode-line-powerline-theme
;  :ensure t)

;; one dark mode line theme
(use-package smart-mode-line-atom-one-dark-theme
  :ensure t)

;; nicer mode-line
(use-package smart-mode-line
  :ensure t
  :config
  (setq sml/theme 'atom-one-dark)
  ;(setq sml/theme 'powerline)
  (add-hook 'after-init-hook 'sml/setup))

;; hide minor modes from the modeline
(use-package diminish
  :ensure t)

;; =========== font stuff ==========

;; set font
(set-frame-font "Hasklug Nerd Font 11" nil t)

;; use utf-8
(prefer-coding-system 'utf-8)
(set-default-coding-systems 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)





;; ========== misc ==========

;; tell emacs who I am, useful for git integration and stuff
(setq user-full-name "Zander Havgaad"
      user-mail-address "zander@havgaard.dk")

;; increase garbage collection threshold to modern range
(setq gc-cons-threshold 50000000)

;; increase threshold for opening large file warning...
(setq large-file-warning-threshold 100000000)

;; use x clipboard
(setq x-select-enable-clipboard t)

;; dont leave temporary/backup files all over the place
(setq backup-directory-alist
      `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms
      `((".*" ,temporary-file-directory t)))

;; change "yes" or "no" prompts to "y" or "n"
(fset 'yes-or-no-p 'y-or-n-p)

;; automatically reload files if they were changed by other programs
(global-auto-revert-mode t)

;; set tabs to be 4 spaces
(setq-default tab-width 4
              indent-tabs-mode nil)

;; when closing files, remove excess whitepace
(add-hook 'before-save-hook 'whitespace-cleanup)

;; better dealing with delimiter pairs
(use-package smartparens
  :ensure t
  :diminish smartparens-mode
  :config
  (progn
    (require 'smartparens-config)
    (smartparens-global-mode 1)
    (show-paren-mode t)))

;; suggest keys when making chords
(use-package which-key
  :ensure t
  :diminish which-key-mode
  :config
  (which-key-mode +1))

;; autocompletion suggestions
(use-package company
  :ensure t
  :diminish company-mode
  :config
  (add-hook 'after-init-hook #'global-company-mode))

;; linting / static code checking
(use-package flycheck
  :ensure t
  :diminish flycheck-mode
  :config
  (add-hook 'after-init-hook #'global-flycheck-mode))

;; jump to definition using the_silver_searcher + ripgrep to try to find declaration
(use-package dumb-jump
  :ensure t)

;; git integration
(use-package magit
  :bind (("C-M-g" . magit-status)))

;; project management, seems useful, not quite sure what it does yet...
;; (use-package projectile
;;   :ensure t
;;   :diminish projectile-mode
;;   :bind
;;   (("C-c p f" . helm-projectile-find-file)
;;    ("C-c p p" . helm-projectile-switch-project)
;;    ("C-c p s" . projectile-save-project-buffers))
;;   :config
;;   (projectile-mode +1)
;; )


;; fuzzy completion framework
(use-package helm
  :ensure t
  :defer 2
  :bind
  ("M-x" . helm-M-x)
  ("C-x C-f" . helm-find-files)
  ("M-y" . helm-show-kill-ring)
  ("C-x b" . helm-mini)
  :config
  (require 'helm-config)
  (helm-mode 1)
  (setq helm-split-window-inside-p t
    helm-move-to-line-cycle-in-source t)
  (setq helm-autoresize-max-height 0)
  (setq helm-autoresize-min-height 20)
  (helm-autoresize-mode 1)
  (define-key helm-map (kbd "<tab>") 'helm-execute-persistent-action) ; rebind tab to run persistent action
  (define-key helm-map (kbd "C-i") 'helm-execute-persistent-action) ; make TAB work in terminal
  (define-key helm-map (kbd "C-z")  'helm-select-action) ; list actions using C-z
  )


;; combine projectile and helm
;; (use-package helm-projectile
;;   :ensure t
;;   :config
;;   (helm-projectile-on))



;; run emacs as a deamon
; (require 'server)
; (if (not (server-running-p)) (server-start))

;; ========== keybindings ==========

;; skip prompt and kill current buffer
(global-set-key (kbd "C-x k") 'kill-this-buffer)



;; ============================================================
;; here be auto generated dragons...


(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   (quote
    ("f2b83b9388b1a57f6286153130ee704243870d40ae9ec931d0a1798a5a916e76" "0d087b2853473609d9efd2e9fbeac088e89f36718c4a4c89c568dd1b628eae41" "071f5702a5445970105be9456a48423a87b8b9cfa4b1f76d15699b29123fb7d8" "84d2f9eeb3f82d619ca4bfffe5f157282f4779732f48a5ac1484d94d5ff5b279" "bc75dfb513af404a26260b3420d1f3e4131df752c19ab2984a7c85def9a2917e" "3c83b3676d796422704082049fc38b6966bcad960f896669dfc21a7a37a748fa" default)))
 '(package-selected-packages
   (quote
    (smart-mode-line-atom-one-dark-theme smart-mode-line-atome-one-dark-theme use-package smart-mode-line doom-themes))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
