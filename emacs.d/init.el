(require 'cl)

(defvar packages
  '(color-theme
    zenburn
    rainbow-mode
    haskell-mode
    erlang))

(defvar configs
  '("global"
    "haskell"
    "erlang"
    "evil"))

(loop for name in packages
      do (progn (unless (fboundp name)
                  (add-to-list 'load-path
                               (concat (file-name-directory (or load-file-name
                                                                (buffer-file-name)))
                                       "packages/"
                                       (symbol-name name)))
                  (require name))))

(add-to-list 'load-path
             (concat (file-name-directory load-file-name)
                     "packages/"))

(loop for name in configs
      do (load (concat (file-name-directory load-file-name)
                       "configs/"
                       name ".el")))

(show-paren-mode 1)
(line-number-mode 1)
(column-number-mode 1)
(size-indication-mode 1)
(transient-mark-mode 1)
(delete-selection-mode 1)

(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)
(blink-cursor-mode -1)

;; page-break-mode
(load "~/.emacs.d/modes/site-page-break-mode.el")

;; opal mode
;;(setq load-path (cons "/data/opal/lib/emacs" load-path))
;;(defvar opal-novice t)
;;(require 'opal-mode)

;; gnus
(setq-default
 gnus-summary-line-format "%U%R%z %(%&user-date;  %-15,15f  %B%s%)\n"
 gnus-user-date-format-alist '((t . "%Y-%m-%d %H:%M"))
 gnus-summary-thread-gathering-function 'gnus-gather-threads-by-references
 gnus-thread-sort-functions '(gnus-thread-sort-by-date)
 gnus-sum-thread-tree-false-root ""
 gnus-sum-thread-tree-indent " "
 gnus-sum-thread-tree-leaf-with-other "├► "
 gnus-sum-thread-tree-root ""
 gnus-sum-thread-tree-single-leaf "╰► "
 gnus-sum-thread-tree-vertical "│")
nil

;; ido-mode
(require 'ido)
(ido-mode t)
(setq ido-enable-flex-matching t)

;; sml-mode
(defun my-sml-mode-hook ()
  (electric-indent-mode 1))
(add-hook 'sml-mode-hook 'my-sml-mode-hook)
