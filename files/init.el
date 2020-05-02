;; TODO: Rust mode stuff
;; TODO: Try solarized-wombat-dark-theme.el
;; TODO: Customize gdb?
;; TODO: eshell-previous-input, eshell-next-input

;; Enable the MELPA package repo
(require 'package)
(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/"))
(package-initialize)

;; Update list of packages asynchronously
(package-refresh-contents t)

;; naysayer-theme
(setq my-packages
      '(evil
        evil-leader
        centered-cursor-mode
        gruvbox-theme
        solarized-theme
        hl-todo
        evil-collection
        powerline
        fish-mode
        magit
        evil-magit))

;; Must be set before require
(setq evil-want-C-u-scroll t
      evil-want-keybinding nil)

;; Load and require the list of packages.
(dolist (pkg my-packages)
  (unless (package-installed-p pkg)
    (package-install pkg))
  (require pkg))

;; Load theme without asking for confirmation
(load-theme 'solarized-wombat-dark t)

;; Set the default font
(set-face-attribute 'default nil
                    :family "Monospace"
                    :height 110
                    :weight 'normal
                    :width 'normal)

;; Make the cursor green.
;(set-cursor-color "#00ff00")

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
  "i" (lambda () ;; Open init.el
        (interactive)
        (find-file user-init-file))
  "I" 'eval-buffer)

;; Enable evil mode.
(evil-mode 1)

(setq evil-collection-setup-minibuffer t)

;; Set all of the evil-collection bindings as once.
(evil-collection-init)

;; Make searching with / or ? not wrap around.
(setq evil-search-wrap nil)

;; Make evil treat emacs symbols as words.
;; For example, "foo-bar" is a symbol in lisp, and "foo_bar" is a symbol in C.
;(with-eval-after-load 'evil
;    (defalias #'forward-evil-word #'forward-evil-symbol)
;    ;; make evil-search-word look for symbol rather than word boundaries
;    (setq-default evil-symbol-word-search t))

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

;; Disable menu bar
(menu-bar-mode -1)

;; Disable scroll bar.
(toggle-scroll-bar -1)

;; Enables line numbers.
(global-linum-mode 1)

;; Highlight TODOs and similar keywords.
(global-hl-todo-mode 1)

;; Get whitespace mode to show only the important stuff.
(setq whitespace-style
      '(tabs
        tab-mark
        trailing
        space-after-tab
        space-before-tab))
(global-whitespace-mode 1)

;; For consistency with evil.
;; Might cause issues, idk.
;; (This is the same as C-g)
(global-set-key (kbd "<escape>") 'keyboard-escape-quit)

;; Show matching paren on hover, with no delay (must be in this order)
(setq show-paren-delay 0)
(show-paren-mode 1)

;; Change the C formatting style for c-like languages.
;; I would rather shoot myself than use the default GNU style.
(setq c-default-style "linux"
      c-basic-offset 4)

;; Set the default powerline theme
(powerline-default-theme)

;; Mousing over a window causes it to gain focus, without having to click on it.
(setq mouse-autoselect-window t)

;; Spaces > Tabs
(setq-default indent-tabs-mode nil)

;; Don't show the startup file
(setq inhibit-startup-screen t)

;; Empty scratch buffer with fundamental-mode.
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

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-names-vector
   ["#3c3836" "#fb4934" "#b8bb26" "#fabd2f" "#83a598" "#d3869b" "#8ec07c" "#ebdbb2"])
 '(custom-safe-themes
   (quote
    ("b89ae2d35d2e18e4286c8be8aaecb41022c1a306070f64a66fd114310ade88aa" "c684e64b79a2fa042fa912b70ba14cd8da0a7175c06ac6791efee57b3209da50" default)))
 '(package-selected-packages
   (quote
    (solarized-theme evil-magit magit fish-mode powerline powerline-theme evil-collection gruvbox-theme evil-leader centered-cursor-mode))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
