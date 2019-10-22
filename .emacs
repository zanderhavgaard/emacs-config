;;
;;
;;  ▓█████  ███▄ ▄███▓ ▄▄▄       ▄████▄    ██████
;;   ▓█   ▀ ▓██▒▀█▀ ██▒▒████▄    ▒██▀ ▀█  ▒██    ▒
;;   ▒███   ▓██    ▓██░▒██  ▀█▄  ▒▓█    ▄ ░ ▓██▄
;;   ▒▓█  ▄ ▒██    ▒██ ░██▄▄▄▄██ ▒▓▓▄ ▄██▒  ▒   ██▒
;;   ░▒████▒▒██▒   ░██▒ ▓█   ▓██▒▒ ▓███▀ ░▒██████▒▒
;;   ░░ ▒░ ░░ ▒░   ░  ░ ▒▒   ▓▒█░░ ░▒ ▒  ░▒ ▒▓▒ ▒ ░
;;    ░ ░  ░░  ░      ░  ▒   ▒▒ ░  ░  ▒   ░ ░▒  ░ ░
;;      ░   ░      ░     ░   ▒   ░        ░  ░  ░
;;      ░  ░       ░         ░  ░░ ░            ░
;;                               ░
;;
;; https://github.com/zanderhavgaard/emacs-config


;; ========== package stuff ==========

;; use package manager
(require 'package)

;; add add-ons repos
(add-to-list 'package-archives '("gnu" . "https://elpa.gnu.org/packages/"))
(add-to-list 'package-archives '("org" . "https://orgmode.org/elpa/"))
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))
(add-to-list 'package-archives '("melpa-stable" . "https://stable.melpa.org/packages/"))

;; initialize packages
(package-initialize)

;; make sure that use-package is installed
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(eval-when-compile
  (require 'use-package))

;; ========== choose to be evil ==========

;; enable evil-leader before evil, so it is avilable in every evil buffer
(use-package evil-leader
  :ensure t
  :config
  (evil-leader/set-leader "SPC")
  (global-evil-leader-mode))

(use-package evil
  :ensure t
  :config
  (evil-mode t))

(use-package evil-nerd-commenter
  :ensure t
  :config
  (evilnc-default-hotkeys))


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
  (doom-themes-neotree-config)
  (doom-themes-org-config)
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

;; nicer modeline
(use-package doom-modeline
  :ensure t
  :hook (after-init . doom-modeline-mode))

;; hide minor modes from the modeline
(use-package diminish
  :ensure t)

;; draw a nice vertical line instead of pagebreak char
(use-package page-break-lines
  :ensure t)

;; add icons
(use-package all-the-icons
  :ensure t)

;; nicer splash screen
(use-package dashboard
  :ensure t
  :config
  (setq dashboard-banner-logo-title "EVIL mode is the only mode!")
  (setq dashboard-startup-banner 'logo)
  (setq dashboard-set-file-icons t)
  (setq dashboard-set-heading-icons t)
  (dashboard-setup-startup-hook))

;; contrast non-editor buffers
(use-package solaire-mode
  :ensure t
  :hook
  ((change-major-mode after-revert ediff-prepare-buffer) . turn-on-solaire-mode)
  (minibuffer-setup . solaire-mode-in-minibuffer)
  :config
  (solaire-global-mode +1)
  (solaire-mode-swap-bg))

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
(setq-default tab-width 2
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
  :ensure t
  :bind (("C-M-g" . magit-status)))

;; show git changes in the fringe
(use-package diff-hl
  :ensure t
  :config
  (add-hook 'prog-mode-hook 'diff-hl-mode))

;; rainbows!
(use-package rainbow-delimiters
  :ensure t
  :config
  (add-hook 'prog-mode-hook 'rainbow-delimiters-mode))

;; project management, seems useful, not quite sure what it does yet...
(use-package projectile
  :ensure t
  :diminish projectile-mode
  :bind
  ;; (("C-c p f" . helm-projectile-find-file)
  ;;  ("C-c p p" . helm-projectile-switch-project)
  ;;  ("C-c p s" . projectile-save-project-buffers))
  :config
  (projectile-mode +1)
)


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
(use-package helm-projectile
  :ensure t
  :config
  (helm-projectile-on))

(use-package neotree
  :ensure t
  :config
  (setq-default neo-show-hidden-files t)
  (global-set-key [f8] 'neotree-toggle)
  (setq neo-theme (if (display-graphic-p) 'icons 'arrow))
  (evil-leader/set-key
    "m"  'neotree-toggle
    "n"  'neotree-project-dir)

  (setq projectile-switch-project-action 'neotree-projectile-action)
  (add-hook 'neotree-mode-hook
            (lambda ()
              (define-key evil-normal-state-local-map (kbd "q") 'neotree-hide)
              (define-key evil-normal-state-local-map (kbd "I") 'neotree-hidden-file-toggle)
              (define-key evil-normal-state-local-map (kbd "z") 'neotree-stretch-toggle)
              (define-key evil-normal-state-local-map (kbd "R") 'neotree-refresh)
              (define-key evil-normal-state-local-map (kbd "m") 'neotree-rename-node)
              (define-key evil-normal-state-local-map (kbd "c") 'neotree-create-node)
              (define-key evil-normal-state-local-map (kbd "d") 'neotree-delete-node)

              (define-key evil-normal-state-local-map (kbd "s") 'neotree-enter-vertical-split)
              (define-key evil-normal-state-local-map (kbd "S") 'neotree-enter-horizontal-split)

              (define-key evil-normal-state-local-map (kbd "RET") 'neotree-enter))))

;; run emacs as a deamon
; (require 'server)
; (if (not (server-running-p)) (server-start))

;; ========== keybindings ==========

;; skip prompt and kill current buffer
(global-set-key (kbd "C-x k") 'kill-this-buffer)

;; easier switching between splits
(define-key evil-normal-state-map (kbd "C-k") 'evil-window-up)
(define-key evil-normal-state-map (kbd "C-j") 'evil-window-down)
(define-key evil-normal-state-map (kbd "C-l") 'evil-window-right)
(define-key evil-normal-state-map (kbd "C-h") 'evil-window-left)


;; ============================================================
;; here be auto generated dragons...


(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   (quote
    ("2d1fe7c9007a5b76cea4395b0fc664d0c1cfd34bb4f1860300347cdad67fb2f9" "f2b83b9388b1a57f6286153130ee704243870d40ae9ec931d0a1798a5a916e76" "0d087b2853473609d9efd2e9fbeac088e89f36718c4a4c89c568dd1b628eae41" "071f5702a5445970105be9456a48423a87b8b9cfa4b1f76d15699b29123fb7d8" "84d2f9eeb3f82d619ca4bfffe5f157282f4779732f48a5ac1484d94d5ff5b279" "bc75dfb513af404a26260b3420d1f3e4131df752c19ab2984a7c85def9a2917e" "3c83b3676d796422704082049fc38b6966bcad960f896669dfc21a7a37a748fa" default)))
 '(package-selected-packages
   (quote
    (doom-modeline smart-mode-line-atom-one-dark-theme smart-mode-line-atome-one-dark-theme use-package smart-mode-line doom-themes))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
