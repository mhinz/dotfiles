(when (< emacs-major-version 24)
  (princ "Install at least Emacs 24.\n" #'external-debugging-output)
  (kill-emacs))

(when (member "Source Code Pro" (font-family-list))
  (set-default-font "Source Code Pro-13"))

(if (fboundp 'tool-bar-mode)     (tool-bar-mode 0))
(if (fboundp 'toggle-scroll-bar) (toggle-scroll-bar 0))
(menu-bar-mode 0)

(setq ring-bell-function 'ignore)
(setq inhibit-splash-screen t)
(setq inhibit-startup-message t)
(setq custom-file "~/.emacs.d/custom.el")

(require 'package)
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/") t)
(add-to-list 'package-archives '("org" . "http://orgmode.org/elpa/") t)
(package-initialize)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(eval-and-compile
  (defvar use-package-verbose t)
  (require 'cl)
  (require 'use-package)
  (require 'bind-key)
  (setq use-package-always-ensure t))

(defalias 'yes-or-no-p 'y-or-n-p)

(setq backup-directory-alist
      `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms
      `((".*" ,temporary-file-directory t)))

(let ((week (* 60 60 24 7))
      (current (float-time (current-time))))
  (dolist (file (directory-files temporary-file-directory t))
    (when (and (backup-file-name-p file)
               (> (- current (float-time (fifth (file-attributes file))))
                  week))
      (message "%s" file)
      (delete-file file))))

(delete-selection-mode +1)

(setq-default tab-width 2)
(setq-default indent-tabs-mode nil)

(when (display-graphic-p)
  (use-package subatomic-theme)
  (toggle-frame-maximized))

(use-package ace-window
  :bind (("M-q" . ace-window)))

(use-package git-gutter
  :config (global-git-gutter-mode 1))

(use-package avy
  :bind* ("C-," . avy-goto-char-2))

(use-package slime
  :init
  (setq inferior-lisp-program "sbcl")
  (setq slime-contribs '(slime-fancy)))

(unless (and (fboundp 'server-running-p)
             (server-running-p))
  (server-start))

(if (file-readable-p custom-file)
    (load custom-file))
