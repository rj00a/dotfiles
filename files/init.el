;; TODO: Rust mode stuff
;; TODO: language server?
;; TODO: emacs server?
;; TODO: Customize gdb?
;; TODO: eshell-previous-input, eshell-next-input?
;; TODO: Press escape to clear search highlight (and exit minibuffer?)

;; Enable the MELPA package repo
(require 'package)
(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/"))
(package-initialize)

;; Update list of packages asynchronously
(package-refresh-contents t)

(setq my-packages
      '(evil
        evil-leader
        centered-cursor-mode
        hl-todo
        evil-collection
        nord-theme
        powerline
        fish-mode
        magit
        evil-magit))

;; Evil tweaks
(setq evil-want-keybinding nil
      evil-want-C-u-scroll t
      evil-want-Y-yank-to-eol t
      evil-want-C-w-in-emacs-state t
      evil-want-minibuffer t)

;; Load and require the list of packages.
(dolist (pkg my-packages)
  (unless (package-installed-p pkg)
    (package-install pkg))
  (require pkg))

;; Load theme without asking for confirmation
(load-theme 'nord t)

;; Set the default font
(set-face-attribute 'default nil
                    :family "Monospace"
                    :height 110
                    :weight 'normal
                    :width 'normal)

;; Make the cursor green.
(set-cursor-color "#00ff00")

;; Make the cursor white.
;(set-cursor-color "#ffffff")

;; Make the cursor never stop blinking.
(setq blink-cursor-blinks 0)

;; Start emacs in fullscreen
;(toggle-frame-fullscreen)

;; Must be set before (evil-mode 1)
(global-evil-leader-mode 1)

(evil-leader/set-key
  "f" 'ido-find-file
  "d" 'ido-dired
  "D" 'cd
  "m" 'mkdir
  "b" 'ido-switch-buffer
  "l" 'buffer-menu
  "k" 'kill-current-buffer
  "K" 'ido-kill-buffer
  "w" 'evil-write
  "W" 'save-some-buffers
  "r" 'revert-buffer
  "s" 'eshell
  "c" 'calculator
  "e" 'eval-expression
  "0" 'delete-trailing-whitespace
  "9" (lambda () ;; Toggle flyspell
        (interactive)
        (if (bound-and-true-p flyspell-mode)
            (turn-off-flyspell)
          (progn
            (flyspell-mode 1)
            (flyspell-buffer))))
  "i" (lambda () ;; Open init.el
        (interactive)
        (find-file user-init-file))
  "I" 'eval-buffer
  "Q" 'kill-emacs)

;; Enable evil mode.
(evil-mode 1)

(setq evil-collection-setup-minibuffer t)

;; Set all of the evil-collection bindings as once.
(evil-collection-init)

;; Make searching with / or ? not wrap around.
'(setq evil-search-wrap nil)

;; When isearching, don't delay before highlighting all matches.
(setq lazy-highlight-initial-delay 0)

;; Change '/' to use evil's search, instead of being a wrapper around isearch.
(evil-select-search-module 'evil-search-module 'evil-search)

(defalias #'forward-evil-word #'forward-evil-symbol)
;; make evil-search-word look for symbol rather than word boundaries
(setq-default evil-symbol-word-search t)

;; Enable centered-cursor-mode globally
(global-centered-cursor-mode 1)

;; Eable Ido mode
(setq ido-enable-flex-matching t
      ido-everywhere t
      ido-cannot-complete-command 'ido-next-match)
(ido-mode 1)

;; Disable tool bar.
(tool-bar-mode -1)

;; Disable menu bar.
;(menu-bar-mode -1)

;; Disable scroll bar.
(toggle-scroll-bar -1)

;; Enables line numbers.
(global-linum-mode 1)

;; Highlight TODOs and similar keywords.
(global-hl-todo-mode 1)

;; Better word wrapping behavior.
(global-visual-line-mode 1)

;; Auto revert files.
(global-auto-revert-mode 1)

;; Auto revert dired
(setq global-auto-revert-non-file-buffers t)

;; Get whitespace mode to show only the important stuff.
(setq whitespace-style
      '(tabs
        tab-mark
        trailing
        space-after-tab
        space-before-tab))
(global-whitespace-mode 1)

;; Dired: Disable warning on use.
(put 'dired-find-alternate-file 'disabled nil)

; ;;; esc quits
; (defun minibuffer-keyboard-quit ()
;   "Abort recursive edit.
; In Delete Selection mode, if the mark is active, just deactivate it;
; then it takes a second \\[keyboard-quit] to abort the minibuffer."
;   (interactive)
;   (if (and delete-selection-mode transient-mark-mode mark-active)
;       (setq deactivate-mark  t)
;     (when (get-buffer "*Completions*") (delete-windows-on "*Completions*"))
;     (abort-recursive-edit)))
; (define-key evil-normal-state-map [escape] 'keyboard-quit)
; (define-key evil-visual-state-map [escape] 'keyboard-quit)
; (define-key minibuffer-local-map [escape] 'minibuffer-keyboard-quit)
; (define-key minibuffer-local-ns-map [escape] 'minibuffer-keyboard-quit)
; (define-key minibuffer-local-completion-map [escape] 'minibuffer-keyboard-quit)
; (define-key minibuffer-local-must-match-map [escape] 'minibuffer-keyboard-quit)
; (define-key minibuffer-local-isearch-map [escape] 'minibuffer-keyboard-quit)

;; Move by visual lines instead of physical lines
;; (Makes a difference with word wrapping)
(evil-define-key nil evil-normal-state-map
  "j" 'evil-next-visual-line
  "k" 'evil-previous-visual-line)

;; Disable the active search highlight with escape.
(evil-define-key nil evil-normal-state-map
  (kbd "<escape>") 'evil-ex-nohighlight)

;; Show matching paren on hover, with no delay (must be in this order)
(setq show-paren-delay 0)

;; Show matching parenthesis when under cursor
;; Make the matching parenthesis underlined.
(show-paren-mode 1)
(set-face-background 'show-paren-match (face-background 'default))
(set-face-foreground 'show-paren-match "#def")
(set-face-attribute 'show-paren-match nil :weight 'extra-bold)
(set-face-underline 'show-paren-match t)
;; Fixes an issue where the line numbers themselves became bold and
;; underlined when adjacent to a character that had those qualities.
(set-face-underline-p 'linum nil)
(set-face-bold-p 'linum nil)

;; Change the C formatting style for c-like languages.
;; I would rather shoot myself than use the default GNU style.
(setq c-default-style "linux"
      c-basic-offset 4)

;; Set the default powerline theme
(powerline-default-theme)

;; Mousing over a window causes it to gain focus without having to click on it.
(setq mouse-autoselect-window t)

;; Spaces > Tabs
(setq-default indent-tabs-mode nil)

;; Tab = 4 spaces
(setq-default tab-width 4)

;; Don't show the startup file
(setq inhibit-startup-screen t)

;; Empty scratch buffer and enter with fundamental-mode.
(setq initial-scratch-message nil
      initial-major-mode 'fundamental-mode)

;; Suppress the default minibuffer message.
(defun display-startup-echo-area-message ())

;; change CWD to $HOME on startup.
(setq-default default-directory (getenv "HOME"))

;; Change backup file behavior to something reasonable.
;; Backups are placed in ~/.emacs.d/backup/
(setq
 backup-directory-alist `(("." . "~/.emacs.d/backup/"))
 version-control t
 vc-follow-symlinks t
 delete-old-versions 6
 kept-old-versions 2)

;; Stop the messages buffer from appearing.
(setq-default message-log-max nil)
(when (get-buffer "*Messages*")
  (kill-buffer "*Messages*"))

;; Stop the completions buffer from appearing.
(add-hook 'minibuffer-exit-hook
          '(lambda ()
             (let ((buffer "*Completions*"))
               (and (get-buffer buffer)
                    (kill-buffer buffer)))))
