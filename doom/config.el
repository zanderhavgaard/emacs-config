;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!

;; Some functionality uses this to identify you, e.g. GPG configuration, email
;; clients, file templates and snippets. It is optional.
(setq user-full-name "Zander Havgaard"
      user-mail-address "zander@havgaard.dk")

;; Doom exposes five (optional) variables for controlling fonts in Doom:
;;
;; - `doom-font' -- the primary font to use
;; - `doom-variable-pitch-font' -- a non-monospace font (where applicable)
;; - `doom-big-font' -- used for `doom-big-font-mode'; use this for
;;   presentations or streaming.
;; - `doom-symbol-font' -- for symbols
;; - `doom-serif-font' -- for the `fixed-pitch-serif' face
;;
;; See 'C-h v doom-font' for documentation and more examples of what they
;; accept. For example:

(setq doom-font (font-spec :family "MartianMono Nerd Font" :size 16 :weight 'regular))

;; If you or Emacs can't find your font, use 'M-x describe-font' to look them
;; up, `M-x eval-region' to execute elisp code, and 'M-x doom/reload-font' to
;; refresh your font settings. If Emacs still can't find your font, it likely
;; wasn't installed correctly. Font issues are rarely Doom issues!

;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. This is the default:
;; (setq doom-theme 'atom-one-dark)
(setq doom-theme 'doom-vibrant)

;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
(setq display-line-numbers-type t)

;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!
(setq org-directory "~/org/")


;; Whenever you reconfigure a package, make sure to wrap your config in an
;; `after!' block, otherwise Doom's defaults may override your settings. E.g.
;;
;;   (after! PACKAGE
;;     (setq x y))
;;
;; The exceptions to this rule:
;;
;;   - Setting file/directory variables (like `org-directory')
;;   - Setting variables which explicitly tell you to set them before their
;;     package is loaded (see 'C-h v VARIABLE' to look up their documentation).
;;   - Setting doom variables (which start with 'doom-' or '+').
;;
;; Here are some additional functions/macros that will help you configure Doom.
;;
;; - `load!' for loading external *.el files relative to this one
;; - `use-package!' for configuring packages
;; - `after!' for running code after a package has loaded
;; - `add-load-path!' for adding directories to the `load-path', relative to
;;   this file. Emacs searches the `load-path' when you load packages with
;;   `require' or `use-package'.
;; - `map!' for binding new keys
;;
;; To get information about any of these functions/macros, move the cursor over
;; the highlighted symbol at press 'K' (non-evil users must press 'C-c c k').
;; This will open documentation for it, including demos of how they are used.
;; Alternatively, use `C-h o' to look up a symbol (functions, variables, faces,
;; etc).
;;
;; You can also try 'gd' (or 'C-c c d') to jump to their definition and see how
;; they are implemented.

;; enable smooter scrolling
(require 'sublimity)
(require 'sublimity-scroll)
(sublimity-mode 1)
(setq sublimity-scroll-weight 25
      sublimity-scroll-drift-length 5)

;; use fill mode for indent guides
(setq highlight-indent-guides-method 'character)
;; make the color of the highlight more intense
(setq highlight-indent-guides-auto-character-face-perc 100)

;; increase length of allowed git message in magit
(setq git-commit-summary-max-length 999)

;; TODO: fix to make writing helm charts nicer
;; Create a derived major-mode based on yaml-mode
;; (define-derived-mode helm-mode yaml-mode "helm"
;;   "Major mode for editing kubernetes helm templates")

;; (after! eglot
;;   ;; Ensure eglot runs in helm-mode buffers
;;   (add-hook 'helm-mode-hook #'eglot-ensure)
;;   ;; Configure `helm_ls serve` for helm-mode
;;   (add-to-list 'eglot-server-programs '(helm-mode "helm_ls" "serve")))

;; save buffers to backup files
;; (setq auto-save-mode 1)
;; save buffer on focus loss
;; (setq auto-save-visited-mode 1)

;; enable super-save mode to auto-save buffers
(super-save-mode +1)
(setq super-save-auto-save-when-idle t)

;; enable tree-sitter mode for all supported modes
(setq +tree-sitter-hl-enabled-modes t)

;; enable word-wrap (almost) everywhere
(+global-word-wrap-mode +1)
