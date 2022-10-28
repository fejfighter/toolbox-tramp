;; Author: Jeff Walsh <fejfighter@gmail.com>  -*- lexical-binding: t; -*-
;; Keywords: convenience, tools
;; Version: 0.5.0

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Provide a `toolbox-first` mode that always finds files inside the
;; context of a toolbox.  This is particularly useful on newer
;; 'immutable' distributions like fedora silverblue/kinoite/coreOS/
;; <next rpm-ostree derived thing> and Suse MicroOS.
;; This works in tandem with toolbox-tramp.el to hook and re-open files 

;;; Code:

(require 'toolbox-tramp)

(defvar toolbox-connected-container nil)

(defun toolbox-select-container (container)
  (interactive
   (list (completing-read "Which Container:" (toolbox-tramp-toolbox-containers))))
  (setq toolbox-connected-container container)
  (toolbox-tramp-reopen-file-in-toolbox (current-buffer) container))

(defun toolbox-get-current-container ()
  toolbox-connected-container)

(defgroup toolbox nil
  "Lock emacs to operate in a container"
  :version "28.1"
  :group 'tools)

(defcustom toolbox-mode-default-container toolbox-tramp-default-container
  "The string to use in the Toolbox mode line."
  :type 'string
  :version "28.1")

(defcustom toolbox-mode-line-lighter "Toolbox"
  "The string to use in the Toolbox mode line."
  :type 'string
  :version "28.1")

(defun toolbox-mode--line-title ()
  `(:propertize
    ,toolbox-mode-line-lighter
    mouse-face mode-line-highlight))

(defvar toolbox-mode-line-title `(:eval (toolbox-mode--line-title))
  "Mode-line construct to show Flymake's mode name and menu.")

(defvar toolbox-mode-line-container `(:eval (toolbox-get-current-container))
  "Mode-line construct to show Flymake's mode name and menu.")

(defcustom toolbox-mode-line-format
  '(" "  toolbox-mode-line-title "[" toolbox-mode-line-container "]")
  "Mode line construct for customizing Toolbox Connection information."
  :type '(repeat (choice string symbol)))

(put 'toolbox-mode-line-format 'risky-local-variable t)
(put 'toolbox-mode-line-title 'risky-local-variable t)
(put 'toolbox-mode-line-container 'risky-local-variable t)

(defvar toolbox-mode-line-container-name '(:eval (toolbox-mode--line-container))
  "Mode-line construct for counting Toolbox diagnostics.
The counters are only placed if some Toolbox backend initialized
correctly.")

(defcustom toolbox-container-name-format
  '("["
    toolbox-connected-container
    "]")
  "Nothing at all "
  :type '(repeat (choice string symbol)))

(defun toolbox-mode--line-container ()
  toolbox-container-name-format)

(defun toolbox-mode-find-file-advisor (args)
  "documentation string"
  (let ((filename . ((car args))))
    (if (file-remote-p filename) args
    (push (toolbox-tramp--path-for-buffer (car args) toolbox-connected-container) (cdr args)))))

(define-minor-mode toolbox-mode "A mode for working inside the confines of a toolbox container"
  :global t :group 'toolbox :require 'toolbox-tramp
  :lighter toolbox-mode-line-format
  (cond
   ;; Turning the mode ON.
   (toolbox-mode
    (advice-add 'find-file :filter-args #'toolbox-mode-find-file-advisor)
    (setq toolbox-connected-container toolbox-mode-default-container)
    (toolbox-tramp-start-toolbox toolbox-connected-container))
   ;; Turning the mode OFF.
   (t
    (advice-remove 'find-file #'toolbox-mode-find-file-advisor))))


