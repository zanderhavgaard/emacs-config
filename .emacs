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
;; Zander's Emacs config
;; https://github.com/zanderhavgaard/emacs-config


;; ========== package management stuff ==========

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

;; install use-package
(eval-when-compile
  (require 'use-package))

;; Enable defer and ensure by default for use-package
(setq use-package-always-ensure t)

;; ========== choose to be evil ==========

;; enable evil-leader before evil, so it is avilable in every evil buffer
(use-package evil-leader
  :config
  (evil-leader/set-leader "SPC")
  (global-evil-leader-mode))

(use-package evil
  :config
  (evil-mode t))

(use-package evil-nerd-commenter
  :config
  (evil-leader/set-key
    "c SPC" 'evilnc-comment-or-uncomment-lines
    "c p" 'evilnc-comment-or-uncomment-paragraphs
    )
  )

(use-package evil-multiedit
  :config
  (evil-multiedit-default-keybinds)
  )

;; =========== theme ==========

;; the doom pack has many nice themes
(use-package doom-themes
  :config
  (setq doom-themes-enable-bold t
    doom-themes-enable-italic t)
  (load-theme 'doom-dracula t)
  (doom-themes-visual-bell-config)
  (doom-themes-neotree-config)
  ;; (doom-themes-treemacs-config)
  (doom-themes-org-config)
  )


;; =========== ui stuff ==========

;; hide unnessecary ui elements
(scroll-bar-mode -1)
(tool-bar-mode -1)
(menu-bar-mode -1)

;; highlight current line
(global-hl-line-mode +1)
;; enable line numbers in the modeline
(global-display-line-numbers-mode 1)
;; use relative line numbers
(setq display-line-numbers-type 'relative)
;; enable line numbers in the margin
(line-number-mode +1)
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
  :hook (after-init . doom-modeline-mode))

;; draw a nice vertical line instead of pagebreak char
(use-package page-break-lines)

;; add icons
(use-package all-the-icons)

;; nicer splash screen
(use-package dashboard
  :config
  (setq dashboard-banner-logo-title "EVIL mode is the only mode!")
  (setq dashboard-startup-banner 'logo)
  (setq dashboard-set-file-icons t)
  (setq dashboard-set-heading-icons t)
  (setq dashboard-center-content t)
  (setq dashboard-show-shortcuts nil)
  (setq dashboard-set-init-info nil)
  (setq dashboard-items '((projects . 20)
                          (recents . 20)
                          ;; (agenda . 10)
                          ))
  (dashboard-setup-startup-hook)
  )

;; contrast non-editor buffers
(use-package solaire-mode
  :hook
  ((change-major-mode after-revert ediff-prepare-buffer) . turn-on-solaire-mode)
  (minibuffer-setup . solaire-mode-in-minibuffer)
  :config
  (solaire-global-mode +1)
  (solaire-mode-swap-bg))

;; draw indent guides #need for yaml...
(use-package highlight-indent-guides
  :config
  (setq highlight-indent-guides-method 'character)
  (add-hook 'prog-mode-hook 'highlight-indent-guides-mode))

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

;; auto-save all buffers of a window when losing focus / changing window focus
(defun save-all ()
  (interactive)
  (save-some-buffers t))
(add-hook 'focus-out-hook 'save-all)

;; change "yes" or "no" prompts to "y" or "n"
(fset 'yes-or-no-p 'y-or-n-p)

;; automatically reload files if they were changed by other programs
(global-auto-revert-mode t)

;; set tabs to be 4 spaces
(setq-default tab-width 2
              indent-tabs-mode nil)

;; when closing files, remove excess whitepace
(add-hook 'before-save-hook 'whitespace-cleanup)

;; Actually a working terminal??
(use-package vterm)

;; better dealing with delimiter pairs
(use-package smartparens
  :config
  (progn
    (require 'smartparens-config)
    (smartparens-global-mode 1)
    (show-paren-mode t)))

;; suggest keys when making chords
(use-package which-key
  :config
  (which-key-mode +1))

;; autocompletion suggestions
(use-package company
  :config
  (add-hook 'after-init-hook #'global-company-mode))

;; linting / static code checking
(use-package flycheck
  :config
  (add-hook 'after-init-hook #'global-flycheck-mode)
  (add-hook 'sh-mode-hook 'flycheck-mode)
  )

;; show flycheck messages inline using minibuffer -> works in cli mode
(use-package flycheck-popup-tip
  :config
  (with-eval-after-load 'flycheck (flycheck-popup-tip-mode))
  )

;; show flymake messages inline using gui
;; (use-package flycheck-pos-tip
;;   :config
;;   (with-eval-after-load 'flycheck
;;     (flycheck-pos-tip-mode))
;;   )

;; use either mode depending on whether in gui or cli mode
;; (eval-after-load 'flycheck
;;   (if (display-graphic-p)
;;       (flycheck-pos-tip-mode)
;;     (flycheck-popup-tip-mode)))

;; jump to definition using the_silver_searcher + ripgrep to try to find declaration
(use-package dumb-jump)

;; git integration
(use-package magit
  :config
  (evil-leader/set-key
    "g s" 'magit-status
    )
  )

;; show git changes in the fringe
(use-package diff-hl
  :config
  (add-hook 'prog-mode-hook 'diff-hl-mode))

;; rainbows!
(use-package rainbow-delimiters
  :config
  (add-hook 'prog-mode-hook 'rainbow-delimiters-mode))

;; fuzzy completion framework
(use-package helm
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
  :config
  (helm-projectile-on))

;; project management, seems useful, not quite sure what it does yet...
(use-package projectile
  :config
  (evil-leader/set-key
    "p p" 'helm-projectile-switch-project
    "p f" 'helm-projectile-find-file
    "p s" 'helm-projectile-save-buffers
    )
  (projectile-mode +1))

;; side panel file browser
(use-package neotree
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
              (define-key evil-normal-state-local-map (kbd "RET") 'neotree-enter)))
  (setq neo-window-fixed-size nil)
  (defun neotree-project-dir ()
    "Open NeoTree using the git root."
    (interactive)
    (let ((project-dir (projectile-project-root))
          (file-name (buffer-file-name)))
      (neotree-toggle)
      (if project-dir
          (if (neo-global--window-exists-p)
              (progn
                (neotree-dir project-dir)
                (neotree-find file-name)))
        (message "Could not find git project root."))))
  )

;; smoother scrolling
(use-package sublimity
  :config
  (require 'sublimity)
  (require 'sublimity-scroll)
  (sublimity-mode 1)
  (setq sublimity-scroll-weight 10
        sublimity-scroll-drift-length 5)
  )

;; center buffer when only one buffer is displayed
(use-package perfect-margin
  :config
  ;; (perfect-margin-mode 1) ;; enable by default
  (evil-leader/set-key
    "f" 'perfect-margin-mode)
  )

;; display a minimap
(use-package minimap
  :config
  (evil-leader/set-key
    "g" 'minimap-mode)
  )

;; code snippets
(use-package yasnippet)

;; language server protocol integration, provides IDE-like features
(use-package lsp-mode
  ;; Optional - enable lsp-mode automatically in scala files
  :hook (scala-mode . lsp)
  :config (setq lsp-prefer-flymake nil))

;; ui for lsp
(use-package lsp-ui)

;; use comapny mode to display lsp messages
(use-package company-lsp)

;; ========== language specific ==========

;; yaml
(use-package yaml-mode
  :config
  (add-to-list 'auto-mode-alist '("\\.yml\\'" . yaml-mode))
  (add-hook 'yaml-mode-hook
            '(lambda ()
               (define-key yaml-mode-map "\C-m" 'newline-and-indent)))
  )

;; use yamllint with flycheck
(use-package flycheck-yamllint
  :defer t
  :init
  (progn
    (eval-after-load 'flycheck
      '(add-hook 'flycheck-mode-hook 'flycheck-yamllint-setup))))

;; dockerfile
(use-package dockerfile-mode
  :config
  (add-to-list 'auto-mode-alist '("Dockerfile\\'" . dockerfile-mode))
  (add-to-list 'auto-mode-alist '(".*Dockerfile.*" . dockerfile-mode))
  )

;; markdown
(use-package markdown-mode
  :commands (markdown-mode gfm-mode)
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode))
  :init (setq markdown-command "/usr/local/bin/multimarkdown"))

;; scala
(use-package scala-mode
  :mode "\\.s\\(cala\\|bt\\)$")

;; scala build tool integration
(use-package sbt-mode
  :commands sbt-start sbt-command
  :config
  ;; WORKAROUND: https://github.com/ensime/emacs-sbt-mode/issues/31
  ;; allows using SPACE when in the minibuffer
  (substitute-key-definition
   'minibuffer-complete-word
   'self-insert-command
   minibuffer-local-completion-map))

;; lsp integration for java using eclipse JDT language server
(use-package lsp-java
  :config
  (add-hook 'java-mode-hook #'lsp)
  )


;; ========== fix dead keys ==========

(define-key key-translation-map [dead-grave] "`")
(define-key key-translation-map [dead-circumflex] "^")
(define-key key-translation-map [dead-tilde] "~")
;; (define-key key-translation-map [dead-diaeresis] "\"")
;; (define-key key-translation-map [dead-acute] "'")

;; ========== keybindings ==========

;; skip prompt and kill current buffer
(global-set-key (kbd "C-x k") 'kill-this-buffer)

;; easier switching between splits
(define-key evil-normal-state-map (kbd "C-k") 'evil-window-up)
(define-key evil-normal-state-map (kbd "C-j") 'evil-window-down)
(define-key evil-normal-state-map (kbd "C-l") 'evil-window-right)
(define-key evil-normal-state-map (kbd "C-h") 'evil-window-left)


;; ========== deamonize =========

;; run emacs as a deamon
; (require 'server)
; (if (not (server-running-p)) (server-start))


;; ============================================================
;; here be auto generated dragons...
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   (quote
    ("7f74a3b9a1f5e3d31358b48b8f8a1154aab2534fae82c9e918fb389fca776788" "423435c7b0e6c0942f16519fa9e17793da940184a50201a4d932eafe4c94c92d" "0fe9f7a04e7a00ad99ecacc875c8ccb4153204e29d3e57e9669691e6ed8340ce" "d6f04b6c269500d8a38f3fabadc1caa3c8fdf46e7e63ee15605af75a09d5441e" "5e0b63e0373472b2e1cf1ebcc27058a683166ab544ef701a6e7f2a9f33a23726" "428754d8f3ed6449c1078ed5b4335f4949dc2ad54ed9de43c56ea9b803375c23" "0d087b2853473609d9efd2e9fbeac088e89f36718c4a4c89c568dd1b628eae41" "cdb3e7a8864cede434b168c9a060bf853eeb5b3f9f758310d2a2e23be41a24ae" "2878517f049b28342d7a360fd3f4b227086c4be8f8409f32e0f234d129cee925" "001c2ff8afde9c3e707a2eb3e810a0a36fb2b466e96377ac95968e7f8930a7c5" "332e009a832c4d18d92b3a9440671873187ca5b73c2a42fbd4fc67ecf0379b8c" "70cc30fd9d27a8d0d3ae82974ac2c409fd2cd5746470e2246778c6bec2d4857c" "70ed3a0f434c63206a23012d9cdfbe6c6d4bb4685ad64154f37f3c15c10f3b90" "2a3ffb7775b2fe3643b179f2046493891b0d1153e57ec74bbe69580b951699ca" "f951343d4bbe5a90dba0f058de8317ca58a6822faa65d8463b0e751a07ec887c" "b462d00de785490a0b6861807a360f5c1e05b48a159a99786145de7e3cce3afe" "f2b83b9388b1a57f6286153130ee704243870d40ae9ec931d0a1798a5a916e76" "1728dfd9560bff76a7dc6c3f61e9f4d3e6ef9d017a83a841c117bd9bebe18613" "f589e634c9ff738341823a5a58fc200341b440611aaa8e0189df85b44533692b" "f30aded97e67a487d30f38a1ac48eddb49fdb06ac01ebeaff39439997cbdd869" "a2286409934b11f2f3b7d89b1eaebb965fd63bc1e0be1c159c02e396afb893c8" "071f5702a5445970105be9456a48423a87b8b9cfa4b1f76d15699b29123fb7d8" "2d1fe7c9007a5b76cea4395b0fc664d0c1cfd34bb4f1860300347cdad67fb2f9" default)))
 '(package-selected-packages
   (quote
    (yasnippet yaml-mode which-key use-package sublimity solaire-mode smartparens scala-mode sbt-mode rainbow-delimiters perfect-margin neotree minimap magit lsp-ui lsp-java highlight-indent-guides helm-projectile flymake-diagnostic-at-point flycheck-yamllint flycheck-pos-tip flycheck-popup-tip evil-nerd-commenter evil-multiedit evil-leader dumb-jump doom-themes doom-modeline dockerfile-mode diff-hl dashboard company-lsp))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
