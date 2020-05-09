;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!

;; Some functionality uses this to identify you, e.g. GPG configuration, email
;; clients, file templates and snippets.
(setq user-full-name "Ryan Johnson"
      user-mail-address "ryanj00a@gmail.com")

;; Doom exposes five (optional) variables for controlling fonts in Doom. Here
;; are the three important ones:
;;
;; + `doom-font'
;; + `doom-variable-pitch-font'
;; + `doom-big-font' -- used for `doom-big-font-mode'; use this for
;;   presentations or streaming.
;;
;; They all accept either a font-spec, font string ("Input Mono-12"), or xlfd
;; font string. You generally only need these two:
(setq doom-font (font-spec :family "monospace" :size 14))

;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. This is the default:
(setq doom-theme 'doom-dracula)

;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!
(setq org-directory "~/org/")

;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
(setq display-line-numbers-type t)

;; Here are some additional functions/macros that could help you configure Doom:
;;
;; - `load!' for loading external *.el files relative to this one
;; - `use-package' for configuring packages
;; - `after!' for running code after a package has loaded
;; - `add-load-path!' for adding directories to the `load-path', relative to
;;   this file. Emacs searches the `load-path' when you load packages with
;;   `require' or `use-package'.
;; - `map!' for binding new keys
;;
;; To get information about any of these functions/macros, move the cursor over
;; the highlighted symbol at press 'K' (non-evil users must press 'C-c g k').
;; This will open documentation for it, including demos of how they are used.
;;
;; You can also try 'gd' (or 'C-c g d') to jump to their definition and see how
;; they are implemented.

;;;;;;;;;;;;;;;;;;;;

;; TODO: Customize the cursor face to be bright green with black text.
;(set-cursor-color "#00ff00")
;; TODO: Don't highlight the rest of the screen when on the last line of the buffer
;; TODO: Don't continue comments when going on the next line
;; TODO: Bind SPC-k to kill-buffer?

(use-package! centered-cursor-mode
  :config
  (setq ccm-recenter-at-end-of-file t))

;;(after! centered-cursor-mode (global-centered-cursor-mode 1))

;; Disable confirmation prompt when quitting emacs.
(setq confirm-kill-emacs nil)

;; make evil-search-word look for symbol rather than word boundaries.
(setq-default evil-symbol-word-search t)

;; Show matching parens instantly.
(setq show-paren-delay 0)
(show-paren-mode 0)
(show-paren-mode 1) ;; Toggle this so that show-paren-delay is in effect.

;; Don't stop blinking the cursor
(setq blink-cursor-blinks 0)
(blink-cursor-mode 1)

;; Make the matching parenthesis underlined.
(set-face-background 'show-paren-match (face-background 'default))
(set-face-foreground 'show-paren-match "#def")
(set-face-attribute 'show-paren-match nil :weight 'extra-bold)
(set-face-underline 'show-paren-match t)

;(global-centered-cursor-mode 1)
