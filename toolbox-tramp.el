;;; toolbox-tramp.el --- tramp connection to toolbx containers  -*- lexical-binding: t; -*-

;; Copyright (C) 2022  Jeff Walsh

;; Author: Jeff Walsh <fejfighter@gmail.com>
;; Keywords: convenience, tools
;; Version: 0.3.0

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

;;;###autoload
(defcustom toolbox-tramp-toolbox-executable "podman"
  "Path to toolbox (or compatible) executable."
  :type '(choice
	  (const "toolbox")
	  (const "podman"))
  :group 'toolbox-tramp)

;;;###autoload
(defcustom toolbox-tramp-flatpak-wrap nil
  "Connect via `flatpak-spawn'"
  :type '(boolean)
  :group 'toolbox-tramp)

(defconst toolbox-tramp-flatpak-spawn-cmd '("flatpak-spawn" "--host"))

(defun toolbox-tramp-flatpak ()
    "Wrap commands with `flatpak-spawn' when running inside flatpak"
    (if toolbox-tramp-flatpak-wrap
	toolbox-tramp-flatpak-spawn-cmd
      ""))

;;;###autoload
(defconst toolbox-tramp-method "toolbox"
  "Method to connect toolbox containers.")

(defun toolbox-tramp-containers (&optional ignored)
  "Return known toolbox containers."
  (let* ((args . ((append (toolbox-tramp-flatpak) '("toolbox" "list" "-c")))))
    (mapcar (lambda (x) (list nil (cadr (split-string x))))
	    (cdr (apply 'process-lines  args)))))

(defconst toolbox-tramp-toolbox-args '(("enter") ("h5")))
(defconst toolbox-tramp-podman-args '(("exec" "-it") ("-u" "%u") ("%h") ("sh")))

;;;###autoload
(defun toolbox-tramp-login-args ()
  "returns the correct login args for the connection type"
  (if (eq toolbox-tramp-toolbox-executable "toolbox")
    toolbox-tramp-toolbox-args
    toolbox-tramp-podman-args))

;;;###autoload
(defun toolbox-tramp-login-program ()
  "determine the default login string"
  (mapconcat #'identity (append (toolbox-tramp-flatpak)
	  (if (eq toolbox-tramp-toolbox-executable "podman")
	      '("podman")
	    '("toolbox"))) " " ))

;;;###autoload
(defconst toolbox-tramp-completion-function-alist
  '((toolbox-tramp-containers ""))
  "Default list of (FUNCTION FILE) pairs to be examined for toolbox method.")

;;;###autoload
(defun toolbox-tramp-add-method ()
  "Add toolbox tramp method."
  (add-to-list 'tramp-methods
	       `(,toolbox-tramp-method
		 (tramp-login-program      ,(toolbox-tramp-login-program))
		 (tramp-login-args         ,(toolbox-tramp-login-args))
		 (tramp-remote-shell       "/bin/sh")
		 (tramp-remote-shell-args  ("-i" "-c")))))

(defconst toolbox-tramp-default-prefix "fedora-toolbox-")

(defvar toolbox-tramp-default-container
  (if (string= toolbox-tramp-toolbox-executable "podman")
      (with-temp-buffer
	(insert-file-contents "/etc/os-release")
	(keep-lines "VERSION_ID" (point-min) (point-max))
	(concat toolbox-tramp-default-prefix (when (string-match "VERSION_ID=\\(.*\\)" (buffer-string))
					       (match-string 1 (buffer-string)))))
    nil))

(defvar toolbox-tramp-default-user
  (if (string= toolbox-tramp-toolbox-executable "podman")
      (user-login-name)
      nil))

(add-to-list 'tramp-default-host-alist `(,toolbox-tramp-method ,toolbox-tramp-default-user ,toolbox-tramp-default-container))
(add-to-list 'tramp-default-user-alist `("\\`toolbox\\'" nil ,toolbox-tramp-default-user))

;;;###autoload
(eval-after-load 'tramp
  '(progn
     (toolbox-tramp-add-method)
     (tramp-set-completion-function
      toolbox-tramp-method
      toolbox-tramp-completion-function-alist)))

(provide 'toolbox-tramp)
