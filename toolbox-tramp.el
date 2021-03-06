;;; toolbox-tramp.el --- tramp connection to toolbx containers  -*- lexical-binding: t; -*-

;; Copyright (C) 2022  Jeff Walsh

;; Author: Jeff Walsh <fejfighter@gmail.com>
;; Keywords: convenience, tools
;; Version: 0.4.1

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
(eval-when-compile (require 'subr-x))

(defgroup toolbox-tramp nil
  "TRAMP integration for toolbox containers."
  :prefix "toolbox-tramp-"
  :group 'applications
  :link '(url-link :tag "Github" "https://github.com/fejfighter/toolbox-tramp")
  :link '(emacs-commentary-link :tag "Commentary" "toolbox-tramp"))

;;;###autoload
(defcustom toolbox-tramp-flatpak-wrap nil
  "Connect via `flatpak-spawn'"
  :type '(boolean)
  :group 'toolbox-tramp)

(defconst toolbox-tramp-executable "podman")
(defconst toolbox-tramp-flatpak-spawn-cmd '("flatpak-spawn" "--host"))
(defconst toolbox-tramp-podman-args '(("exec" "-it") ("-u" "%u") ("%h") ("sh")))
(defconst toolbox-tramp-podman-list `(,toolbox-tramp-executable "container" "list" "--format={{.Names}}"))
(defconst toolbox-tramp-podman-label-filter '("-f=label=com.github.containers.toolbox=true"))

(defun toolbox-tramp-flatpak ()
    "Wrap commands with `flatpak-spawn' when running inside flatpak"
    (if toolbox-tramp-flatpak-wrap
	toolbox-tramp-flatpak-spawn-cmd
      ""))

(defun toolbox-tramp-toolbox-containers (&optional ignored)
  "Return known running toolbox containers."
  (let* ((args . ((append (toolbox-tramp-flatpak)
			  toolbox-tramp-podman-list
			  toolbox-tramp-podman-label-filter
			  ))))
    (mapcar (lambda (x) (list nil x))
	    (apply 'process-lines args))))



(defun toolbox-tramp-stopped-toolbox-containers (&optional ignored)
  "Return known toolbox stopped containers."
  (let* ((args . ((append (toolbox-tramp-flatpak) toolbox-tramp-podman-list
						    toolbox-tramp-podman-label-filter
						    '("-f=status=exited"
						    "-f=status=created"
						    "-f=status=paused")))))
    (apply 'process-lines  args)))

(defun toolbox-tramp-all-containers (&optional ignored)
  "Return known running podman containers."
  (let* ((args . ((append (toolbox-tramp-flatpak)
			  toolbox-tramp-podman-list))))
    (mapcar (lambda (x) (list nil x))
	    (apply 'process-lines args))))

(defun toolbox-tramp-start-toolbox ()
  (interactive)
  (let ((container . ((completing-read "Start Container" (toolbox-tramp-stopped-toolbox-containers)))))
    (let ((args . ((append (toolbox-tramp-flatpak) `(,toolbox-tramp-executable "container" "start")))))
    	 (apply 'call-process (append (list (car args) nil nil nil) (cdr args) (list container))))))

;;;###autoload
(defun toolbox-tramp-login-args ()
  "returns the correct login args for the connection type"
    toolbox-tramp-podman-args)

;;;###autoload
(defun toolbox-tramp-login-program ()
  "determine the default login string"
  (string-join (append (toolbox-tramp-flatpak) '("podman")) " " ))

;;;###autoload
(defconst podman-tramp-completion-function-alist
  '((toolbox-tramp-all-containers ""))
  "Default list of (FUNCTION FILE) pairs to be examined for podman method.")


;;;###autoload
(defconst toolbox-tramp-completion-function-alist
  '((toolbox-tramp-toolbox-containers ""))
  "Default list of (FUNCTION FILE) pairs to be examined for toolbox method.")

;;;###autoload
(defconst toolbox-tramp-method "toolbox"
  "Method to connect toolbox containers.")

;;;###autoload
(defun toolbox-tramp-add-method ()
  "Add toolbox tramp method."
  (add-to-list 'tramp-methods
	       `(,toolbox-tramp-method
		 (tramp-login-program      ,(toolbox-tramp-login-program))
		 (tramp-login-args         ,(toolbox-tramp-login-args))
		 (tramp-remote-shell       "/bin/sh")
		 (tramp-remote-shell-args  ("-c"))))
)

(defconst toolbox-tramp-default-prefix "fedora-toolbox-")

(defvar toolbox-tramp-default-container
      (with-temp-buffer
	(insert-file-contents "/etc/os-release")
	(keep-lines "VERSION_ID" (point-min) (point-max))
	(concat toolbox-tramp-default-prefix (when (string-match "VERSION_ID=\\(.*\\)" (buffer-string))
					       (match-string 1 (buffer-string))))))

(defvar toolbox-tramp-default-user
      (user-login-name))

(add-to-list 'tramp-default-host-alist `(,toolbox-tramp-method ,toolbox-tramp-default-user ,toolbox-tramp-default-container))
(add-to-list 'tramp-default-user-alist `("\\`toolbox\\'" nil ,toolbox-tramp-default-user))

;;;###autoload
(defconst podman-tramp-method "podman"
  "Method to connect toolbox containers.")

;;;###autoload
(defun podman-tramp-add-method ()
  "Add toolbox tramp method."
  (add-to-list 'tramp-methods
	       `(,podman-tramp-method
		 (tramp-login-program      ,(toolbox-tramp-login-program))
		 (tramp-login-args         ,(toolbox-tramp-login-args))
		 (tramp-remote-shell       "/bin/sh")
		 (tramp-remote-shell-args  ("-c")))))

(add-to-list 'tramp-default-user-alist `("\\`podman\\'" nil ,toolbox-tramp-default-user))

;;;###autoload
(eval-after-load 'tramp
  '(progn
     (toolbox-tramp-add-method)
     (tramp-set-completion-function
      toolbox-tramp-method
      toolbox-tramp-completion-function-alist)
     (podman-tramp-add-method)
     (tramp-set-completion-function
      podman-tramp-method
      podman-tramp-completion-function-alist)))

(provide 'toolbox-tramp)
