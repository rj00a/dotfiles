;; TODO: Make 'K' insert newline in normal mode.
;;   Alternatively, unbind enter, space, and tab in normal mode?
;; TODO: highlight todos
;; TODO: Rust mode stuff
;; TODO: Better C formatting in c-mode
;; TODO: 'e' leader key for ido-find-file (or ivy)
;; TODO: try solarized-wombat-dark-theme.el
;; TODO: try powerline

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
        gruvbox-theme)) ;; rainbow-delimiters, naysayer-theme

(setq evil-want-C-u-scroll t) ;; Must be set before require

;; Load and require the list of packages.
(dolist (pkg my-packages)
  (unless (package-installed-p pkg)
    (package-install pkg))
  (require pkg))

;; Enable rainbow-delimiters-mode in most programming modes
;; (There is no global mode for this)
;(add-hook 'prog-mode-hook #'rainbow-delimiters-mode)

;; Load theme without asking for confirmation
(load-theme 'gruvbox t)

;; Make the cursor green.
;(set-cursor-color "#00ff00")

;; Make the cursor never stop blinking.
(setq blink-cursor-blinks 0)

;; Must be set before (evil-mode 1)
(global-evil-leader-mode 1)

(evil-leader/set-key
  "l" 'buffer-menu
  "k" 'kill-current-buffer
  "f" 'ido-find-file
  "w" 'evil-write
  "W" 'save-some-buffers
  "r" 'revert-buffer
  "s" 'eshell
  "c" 'calculator
  "0" 'delete-trailing-whitespace
  "i" (lambda () ;; Open init.el
        (interactive)
        (find-file user-init-file))
  "I" 'eval-buffer)

;; Enable evil mode.
(evil-mode 1)

;; Make searching with / or ? not wrap around.
(setq evil-search-wrap nil)

;; Make evil treat emacs symbols as words.
;; For example, "foo-bar" is a symbol in lisp, and "foo_bar" is a symbol in C.
(with-eval-after-load 'evil
    (defalias #'forward-evil-word #'forward-evil-symbol)
    ;; make evil-search-word look for symbol rather than word boundaries
    (setq-default evil-symbol-word-search t))

;; Enable centered-cursor-mode globally
(global-centered-cursor-mode 1)

;; Changes the default find-file and switch-to-buffer commands to Ido versions.
(ido-mode 1)

;; Disable tool bar.
(tool-bar-mode -1)

;; Disable menu bar
(menu-bar-mode -1)

;; Disable scroll bar.
(toggle-scroll-bar -1)

;; Enables line numbers.
(global-linum-mode 1)

;; For consistency with evil.
;; Might cause issues, idk.
;; (This is the same as C-g)
(global-set-key (kbd "<escape>") 'keyboard-escape-quit)

;; Show matching paren on hover, with no delay (must be in this order)
(setq show-paren-delay 0)
(show-paren-mode 1)

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
 '(package-selected-packages (quote (gruvbox-theme evil-leader centered-cursor-mode))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
