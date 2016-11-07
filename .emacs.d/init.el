(when (member "Source Code Pro" (font-family-list))
  (set-default-font "Source Code Pro-13"))

(tool-bar-mode 0)
(menu-bar-mode 0)
(toggle-scroll-bar 0)
(toggle-frame-maximized)

(setq ring-bell-function 'ignore)
(setq inhibit-splash-screen t)
(setq inhibit-startup-message t)

(setq custom-file "~/.emacs.d/custom.el")
(load custom-file)

(when (>= emacs-major-version 24)
  (require 'package)
  (add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/") t)
  (add-to-list 'package-archives '("org" . "http://orgmode.org/elpa/") t)
  (package-initialize))

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(eval-and-compile
  (defvar use-package-verbose t)
  (require 'cl)
  (require 'use-package)
  (require 'bind-key)
  (require 'diminish)
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

(if (display-graphic-p)
    (use-package subatomic-theme))

(use-package ace-window
  :bind (("M-q" . ace-window)))

(use-package git-gutter
  :config (global-git-gutter-mode 1))

(use-package avy
  :bind* (("C-'" . avy-goto-char)
          ("C-," . avy-goto-char-2)))
