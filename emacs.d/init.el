(setq package-archives '(("gnu" . "http://elpa.gnu.org/packages/")
			 ("melpa" . "http://melpa.milkbox.net/packages/")))

(setq backup-directory-alist `(("." . "~/.emacs.d/saves"))
     backup-by-copying t
     kept-new-versions 6
     kept-old-versions 2
     version-control t)
;;(autoload 'ghc-init "ghc" nil t)
;;(autoload 'ghc-debug "ghc" nil t)
;;(Add-hook 'haskell-mode-hook 'ghc-init)

(defvar packages
  '(("ace-jump-mode" ace-jump-mode)
    ("async" async)
    ("color-theme" color-theme)
    ("erlang" erlang)
    ("evil" evil)
    ("evil-surround" evil-surround)
    ("evil-visualstar" evil-visualstar)
    ("folding-mode" folding)
    ("haskell-mode" haskell-mode)
    ("helm" helm)
    ("linum-relative" linum-relative)
    ("moe-theme.el" moe-theme)
    ("rainbow-delimiters" rainbow-delimiters)
    ("slime" slime)
    ("structured-haskell-mode/elisp" shm)
    ("undo-tree" undo-tree)
    ("zenburn" zenburn))
  "Packges to be loaded from ~/.emacs.d/packages/.")

(defvar configs
  '("erlang"
    "haskell"
    "lisp")
  "Custom configurations for specific tasks.")

(require 'cl)

(loop for package in packages
      do (progn
	   (add-to-list 'load-path
			(concat (file-name-directory (or load-file-name
							 (buffer-file-name)))
				"packages/"
				(car package)))
	   (require (car (cdr package)))))

(loop for name in configs
      do (load (concat (file-name-directory (or load-file-name
						(buffer-file-name)))
		       "configs/"
		       name ".el")))

(add-to-list 'custom-theme-load-path "~/.emacs.d/themes")

;; GLOBAL CONFIGURATION

(setq inhibit-splash-screen t)
(setq initial-scratch-message nil)
(setq ring-bell-function 'ignore)

(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)
(blink-cursor-mode -1)

(line-number-mode t)
(column-number-mode t)

(show-paren-mode t)
(setq show-paren-delay 0)

(if window-system
    (progn
      (custom-set-faces
       '(default ((t (:inherit nil :height 126 :width normal :family "Monaco")))))
      (load-theme 'subatomic t))
    (load-theme 'wombat t))

(require 'saveplace)
(setq-default save-place t)

(setq linum-relative-format "%3s ")
(setq linum-relative-current-symbol "")

(global-undo-tree-mode)
(setq undo-tree-auto-save-history t)

(evil-mode t)
(global-evil-surround-mode t)
(global-evil-visualstar-mode t)
(defalias 'redo 'undo-tree-redo)
(define-key evil-normal-state-map (kbd "SPC") 'ace-jump-mode)

(load "folding" 'noerror)
(folding-mode-add-find-file-hook)

(global-set-key (kbd "C-h") 'delete-backward-char)
(global-set-key (kbd "M-h") 'help-command)
(global-set-key (kbd "C-c C-m") 'execute-extended-command)
(global-set-key (kbd "C-c C-;") 'eval-expression)
