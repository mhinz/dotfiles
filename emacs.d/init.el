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
    ("structured-haskell-mode/elisp" shm)
    ("linum-relative" linum-relative)
    ("undo-tree" undo-tree)
    ("zenburn" zenburn)))

(defvar configs
  '("erlang"
    "haskell"))

(require 'cl)

(loop for package in packages
      do (progn
	   (add-to-list 'load-path
			(concat (file-name-directory load-file-name)
				"packages/"
				(car package)))
	   (require (car (cdr package)))))

(loop for name in configs
      do (load (concat (file-name-directory load-file-name)
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

(show-paren-mode t)
(line-number-mode t)
(column-number-mode t)

(if window-system
    (progn
      (custom-set-faces
       '(default ((t (:inherit nil :height 130 :width normal :family "Monaco")))))
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
