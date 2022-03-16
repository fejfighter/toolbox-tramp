;;; toolbox-tramp.el --- tramp connection to toolbx containers  -*- lexical-binding: t; -*-

;; Copyright (C) 2022  Jeff Walsh

;; Author: Jeff Walsh <fejfighter@gmail.com>
;; Keywords: convenience, tools
;; Version: 0.2.0

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

;; Connect to podman's toolbox containers, shamelessly influenced
;; by/stolen from docker-tramp.el, just specialized/re-written for
;; toolbox

;;; Code:

(require 'tramp)

(defgroup toolbox-tramp nil
  "TRAMP integration for toolbox containers."
  :prefix "toolbox-tramp-"
  :group 'applications
  :link '(url-link :tag "Github" "https://github.com/fejfighter/toolbox-tramp.el")
  :link '(emacs-commentary-link :tag "Commentary" "toolbox-tramp"))

(defcustom toolbox-tramp-toolbox-executable "toolbox"
  "Path to toolbox (or compatible) executable."
  :type '(choice
          (const "toolbox")
          (const "podman")
	  (const "flatpak-spawn --host podman")
          (string))
  :group 'toolbox-tramp)

;;;###autoload
(defcustom toolbox-tramp-toolbox-options nil
  "List of toolbox options."
  :type '(repeat string)
  :group 'toolbox-tramp)

;;;###autoload
(defconst toolbox-tramp-method "toolbox"
  "Method to connect toolbox containers.")

(defun toolbox-tramp-containers (&optional ignored)
  "Return known toolbox containers."
  (mapcar (lambda (x) (list nil (cadr (split-string x)))) (cdr (process-lines "toolbox" "list" "-c"))))

;;;###autoload
(defconst toolbox-tramp-completion-function-alist
  '((toolbox-tramp-containers ""))
  "Default list of (FUNCTION FILE) pairs to be examined for toolbox method.")


;;;###autoload
(defun toolbox-tramp-add-method ()
  "Add toolbox tramp method."
  (add-to-list 'tramp-methods
               `(,toolbox-tramp-method
                 (tramp-login-program      ,toolbox-tramp-toolbox-executable)
                 (tramp-login-args         (("enter") ("%h")))
                 (tramp-remote-shell       "/bin/sh")
                 (tramp-remote-shell-args  ("-i" "-c")))))

(add-to-list 'tramp-default-host-alist '(,toolbox-tramp-method nil ""))

;;;###autoload
(eval-after-load 'tramp
  '(progn
     (toolbox-tramp-add-method)
     (tramp-set-completion-function
      toolbox-tramp-method
      toolbox-tramp-completion-function-alist)))

(provide 'toolbox-tramp)
