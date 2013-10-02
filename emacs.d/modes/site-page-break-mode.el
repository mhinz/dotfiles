;;; -*- Mode: Emacs-Lisp -*-
;;;
;;; site-page-break-mode.el --- Page break handling
;;;
;;; Time-stamp: <Monday Jan 30, 2012 00:09:47 asmodai>
;;; Revision:   11
;;;
;;; Copyright (c) 2012 Paul Ward <asmodai@gmail.com>
;;;
;;; Author:     Paul Ward <asmodai@gmail.com>
;;; Maintainer: Paul Ward <asmodai@gmail.com>
;;; Created:    29 Jan 2012 15:54:23
;;; Keywords:   
;;; URL:        not distributed yet
;;;
;;;{{{ License:
;;;
;;; This program is free software: you can redistribute it
;;; and/or modify it under the terms of the GNU General Public
;;; Licenseas published by the Free Software Foundation,
;;; either version 3 of the License, or (at your option) any
;;; later version.
;;;
;;; This program isdistributed in the hope that it will be
;;; useful, but WITHOUT ANY  WARRANTY; without even the implied
;;; warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
;;; PURPOSE.  See the GNU General Public License for more
;;; details.
;;;
;;; You should have received a copy of the GNU General Public
;;; License along with this program.  If not, see
;;; <http://www.gnu.org/licenses/>.
;;;
;;;}}}
;;;{{{ Commentary:
;;;
;;;}}}

(defvar page-break-face 'escape-glyph)

(defvar page-break-string-char ?-)

(defvar page-break-string "< Page Break >")

(defun make-page-break-line (window)
  (let ((w-width (window-width window))
        (s-width (length page-break-string)))
    (cond ((<= w-width (+ 3 s-width))
           (make-string (1- (window-width window))
                        page-break-string-char))
          (t
           (concat "--"
                   page-break-string
                   (make-string (- (window-width window)
                                   s-width
                                   2)
                                page-break-string-char))))))

(defun page-break-display-table (window)
  "Create a display-table that displays page-breaks in a pretty way."
  (let ((table (or (copy-sequence (window-display-table window))
                   (make-display-table))))
    (aset table ?\^L
          (vconcat (mapcar (lambda (c)
                             (make-glyph-code c page-break-face))
                           (make-page-break-line window))))
    table))

(defun page-break-mode-hook-function ()
  "Function called for updating display table."
  (mapcar (lambda (window)
            (set-window-display-table
             window
             (page-break-display-table window)))
          (window-list nil 'no-minibuffer)))

;;;###autoload
(define-minor-mode page-break-mode
    "Toggle Page Break mode."
  :global t
  :lighter " Pgbrk"
  (if page-break-mode
      (add-hook 'window-configuration-change-hook
                'page-break-mode-hook-function)
      (remove-hook 'window-configuration-change-hook
                   'page-break-mode-hook-function)))

(defun turn-on-page-break-mode ()
  (page-break-mode 1))

(defun turn-off-page-break-mode ()
  (page-break-mode -1))

(turn-on-page-break-mode)

;;; site-page-break-mode.el ends here
