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
; (add-to-list 'package-archives '("melpa-stable" . "https://stable.melpa.org/packages/"))

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

;; required to be loaded before evil-collection
;; TODO can they be moved to :config section of evil?
;; https://github.com/emacs-evil/evil-collection
(setq evil-want-integration t) ;; This is optional since it's already set to t by default.
(setq evil-want-keybinding nil)

;; enable evil-leader before evil, so it is avilable in every evil buffer
(use-package evil-leader
  :config
  (evil-leader/set-leader "SPC")
  (global-evil-leader-mode))

;; be evil
(use-package evil
  :after evil-collection
  :config
  (evil-mode 1))

;; evil keybindings for modes evil does not cover
(use-package evil-collection
  :after evil
  :init
(setq evil-want-integration t)
(setq evil-want-keybinding nil)
  :config
  (evil-collection-init)
  )

;; evil keybindings for magit
(use-package evil-magit)

;; smart toggle line comments
(use-package evil-nerd-commenter
  :config
  (evil-leader/set-key
    "c SPC" 'evilnc-comment-or-uncomment-lines
    "c p" 'evilnc-comment-or-uncomment-paragraphs
    )
  )

;; multiline cursors with regex
(use-package evil-multiedit
  :config
  (evil-multiedit-default-keybinds)
  )

;; =========== theme ==========

;; ;; nice dark colorful theme
;; (use-package challenger-deep-theme
;;   :config
;;   (load-theme 'challenger-deep t)
;;   ;; (set-face-attribute 'font-lock-comment-face nil :slant 'italic)
;;   (set-face-attribute 'font-lock-function-name-face nil :slant 'italic)
;;   (set-face-attribute 'font-lock-variable-name-face nil :slant 'italic)
;;   )

;; ;; collection of base16 themes
;; (use-package base16-theme
;;   :config
;;   (load-theme 'base16-default-dark t)
;;   ;; (load-theme 'base16-chalk t)
;;   ;; (load-theme 'base16-3024 t)
;;   ;; (set-face-attribute 'font-lock-comment-face nil :slant 'italic)
;;   (set-face-attribute 'font-lock-function-name-face nil :slant 'italic)
;;   (set-face-attribute 'font-lock-variable-name-face nil :slant 'italic)
;;   )

;; the doom pack has many nice themes
(use-package doom-themes
  :config
  (setq doom-themes-enable-bold t
    doom-themes-enable-italic t)
  (load-theme 'doom-one t)
  (doom-themes-visual-bell-config)
  (doom-themes-neotree-config)
  ;; (doom-themes-treemacs-config)
  (doom-themes-org-config)
  ;; add some italics
  ;; (set-face-attribute 'font-lock-comment-face nil :slant 'italic)
  (set-face-attribute 'font-lock-function-name-face nil :slant 'italic)
  (set-face-attribute 'font-lock-variable-name-face nil :slant 'italic)
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

;; doom modeline
(use-package doom-modeline
  :hook (after-init . doom-modeline-mode)
  :config
  ;; (setq doom-modeline-buffer-file-name-style 'truncate-upto-project)
  (setq doom-modeline-buffer-file-name-style 'relative-to-project)
  (setq doom-modeline-project-detection 'projectile)
  (setq doom-modeline-icon (display-graphic-p))
  (setq doom-modeline-evil-state-icon t)
  )

;; ;; spacemacs modeline
;; (use-package spaceline
;;   :config
;;   (require 'spaceline-config)
;;   (spaceline-spacemacs-theme)
;;   )

;; ;; icons for spaceline
;; (use-package spaceline-all-the-icons
;;   :after spaceline
;;   :config
;;   (spaceline-all-the-icons-theme)
;;   ;; (setq spaceline-all-the-icons-separator-type 'slant)
;;   (setq spaceline-highlight-face-func 'spaceline-highlight-face-evil-state)
;;   )

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
  (setq dashboard-items '((projects . 1000)
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
  (add-hook 'prog-mode-hook 'highlight-indent-guides-mode)
  (add-hook 'yaml-mode-hook 'highlight-indent-guides-mode))

;; =========== font stuff ==========

;; set font
(set-frame-font "Hasklug Nerd Font 14" nil t)

;; use utf-8
(prefer-coding-system 'utf-8)
(set-default-coding-systems 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)

;; ========== misc ==========

;; import env vars from the shell
(use-package exec-path-from-shell
  :init
  (exec-path-from-shell-copy-env "WORKON_HOME")
  (exec-path-from-shell-copy-env "PYTHONPATH")
  (exec-path-from-shell-copy-env "CLASSPATH")
  (exec-path-from-shell-initialize)
  )
  ;; TODO fix imports warning message

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

;; show todos in magit status
(use-package magit-todos
  :config
  (magit-todos-mode)
  (evil-leader/set-key
    "t l" 'magit-todos-list
    )
  )

;; highlight todos and quickly jump between todos in file
(use-package hl-todo
  :config
  (global-hl-todo-mode)
  (evil-leader/set-key
    "t p" 'hl-todo-previous
    "t n" 'hl-todo-next
    "t o" 'hl-todo-occur
    "t i" 'hl-todo-insert
    )
  (add-hook 'org-mode-hook 'hl-todo-mode)
  )

;; show color codes as background
(use-package rainbow-mode
  :config
  (evil-leader/set-key
    "h" 'rainbow-mode
    )
  )

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

;; project management
(use-package projectile
  :config
  ;; projects on Soyuz
  (if (file-exists-p "/home/zander/praqma")
      (setq projectile-project-search-path '(
                                             "/home/zander"
                                             "/home/zander/praqma/novo_resume"
                                             ))
    )
  ;; projects on Vostok & Nostromo
   (if (file-exists-p "/home/zander/Dropbox")
      (setq projectile-project-search-path '(
                                             "/home/zander"
                                             "/home/zander/Dropbox/github/"
                                             "/home/zander/Dropbox/ITU/Master/3_semester/advanced_programming/Advanced_programming"
                                             "/home/zander/Dropbox/ITU/Master/3_semester/parallel_concurrent_programming/PCPP"
                                             "/home/zander/Dropbox/ITU/Master/3_semester/applied_algorithms/apalg"
                                             "/home/zander/Dropbox/ITU/Master/3_semester/rp"
                                             ))
     )
  (evil-leader/set-key
    "p p" 'helm-projectile-switch-project
    "p f" 'helm-projectile-find-file
    "p s" 'helm-projectile-save-buffers
    )
  (projectile-mode +1))

;; combine projectile and helm
(use-package helm-projectile
  :config
  (helm-projectile-on))

;; side panel file browser
(use-package neotree
  :config
  (setq-default neo-show-hidden-files t)
  (global-set-key [f8] 'neotree-toggle)
  (setq neo-theme (if (display-graphic-p) 'icons 'arrow))
  (evil-leader/set-key
    "m"  'neotree-toggle
    )
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

  ;; below lines should be obsolete..
  ;; (defun neotree-project-dir ()
  ;;   "Open NeoTree using the git root."
  ;;   (interactive)
  ;;   (let ((project-dir (projectile-project-root))
  ;;         (file-name (buffer-file-name)))
  ;;     (neotree-toggle)
  ;;     (if project-dir
  ;;         (if (neo-global--window-exists-p)
  ;;             (progn
  ;;               (neotree-dir project-dir)
  ;;               (neotree-find file-name)))
  ;;       (message "Could not find git project root."))))
  )

;; scroll one line at a time (less "jumpy" than defaults)
(setq mouse-wheel-scroll-amount '(1 ((shift) . 1))) ;; one line at a time
(setq mouse-wheel-progressive-speed nil) ;; don't accelerate scrolling
(setq mouse-wheel-follow-mouse 't) ;; scroll window under mouse
(setq scroll-step 1) ;; keyboard scroll one line at a time
;; more vim like scrolling...
(setq scroll-conservatively 10000)
(setq auto-window-vscroll nil)

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
    "d" 'minimap-mode)
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

;; ;; erlang
;; ;; projects on Vostok & Nostromo
;; (if (file-exists-p "/home/zander/Dropbox")
;;     (setq load-path (cons  "/usr/lib/erlang/lib/tools-3.2.1/emacs"load-path))
;;   (setq erlang-root-dir "/usr/lib/erlang")
;;   (setq exec-path (cons "/usr/lib/erlang/bin" exec-path))
;;   (require 'erlang-start)
;;   )

;; python
(use-package elpy
  :init
  (elpy-enable)
  ;; (add-hook 'elpy-mode-hook (lambda () (highlight-indentation-mode -1)))
  :config
  (delete 'elpy-module-highlight-indentation elpy-modules)
  (setq elpy-rpc-backend "jedi")
  (evil-leader/set-key
    "y w" 'pyvenv-workon
    )
)

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
    ("70cc30fd9d27a8d0d3ae82974ac2c409fd2cd5746470e2246778c6bec2d4857c" "b3bcf1b12ef2a7606c7697d71b934ca0bdd495d52f901e73ce008c4c9825a3aa" default)))
 '(package-selected-packages
   (quote
    (sbt-mode scala-mode dockerfile-mode flycheck-yamllint yaml-mode elpy company-lsp lsp-ui lsp-mode yasnippet minimap perfect-margin sublimity neotree helm-projectile projectile helm rainbow-delimiters rainbow-mode magit-todos diff-hl dumb-jump flycheck-popup-tip flycheck company which-key smartparens vterm exec-path-from-shell highlight-indent-guides solaire-mode dashboard page-break-lines doom-modeline doom-themes evil-multiedit evil-nerd-commenter evil-magit evil-collection evil-leader use-package))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
