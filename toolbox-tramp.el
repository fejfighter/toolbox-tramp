;;; toolbox-tramp.el --- Tramp connection to toolbx containers  -*- lexical-binding: t; -*-

;; Copyright (C) 2022  Jeff Walsh

;; Author: Jeff Walsh <fejfighter@gmail.com>
;; Keywords: convenience, tools
;; Version: 0.5.0
;; URL: https://github.com/fejfighter/toolbox-tramp
;; Package-Requires: ((eglot "1.9")  (tempel "0.5") (emacs "25.1"))


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

(defconst toolbox-tramp-executable "podman")
(defconst toolbox-tramp-podman-args '(("exec" "-it") ("-u" "%u") ("%h") ("%l")))

(defconst toolbox-tramp-podman-list `(,toolbox-tramp-executable "container" "list" "--format={{.Names}}"))
(defconst toolbox-tramp-podman-label-filter '("-f=label=com.github.containers.toolbox=true"))

(defun toolbox-tramp-toolbox-containers (&optional filename)
  "Return known running toolbox containers.
FILENAME is ignored as this it is sourced from podman"
  (ignore filename)
  (let* ((args . ((append toolbox-tramp-podman-list
			  toolbox-tramp-podman-label-filter))))
	    (apply 'process-lines args)))

(defun toolbox-tramp-toolbox-containers-completion (&optional filename)
  "Return known running toolbox containers for tramp completion.
FILENAME is ignored as this it is sourced from podman"
    (ignore filename)
    (mapcar (lambda (x) (list nil x))
	    (toolbox-tramp-toolbox-containers)))


(defun toolbox-tramp-stopped-toolbox-containers (&optional filename)
  "Return known toolbox stopped containers.
FILENAME is ignored as this it is sourced from podman"
  (ignore filename)
  (let* ((args . ((append toolbox-tramp-podman-list
			  toolbox-tramp-podman-label-filter
			  '("-f=status=exited"
			    "-f=status=created"
			    "-f=status=paused")))))
    (apply 'process-lines args)))

(defun toolbox-tramp-all-containers (&optional filename)
  "Return known running podman containers.
FILENAME is ignored as this it is sourced from podman"
    (ignore filename)
    (mapcar (lambda (x) (list nil x))
	    (apply 'process-lines toolbox-tramp-podman-list)))

(defun toolbox-tramp-start-toolbox (container)
  "Start a toolbox CONTAINER for later connection."
  (interactive
   (list (completing-read "Select container: " (toolbox-tramp-stopped-toolbox-containers))))
    (let ((args . ((append `(,toolbox-tramp-executable "container" "start")))))
      (apply 'call-process (append (list (car args) nil nil nil) (cdr args) (list container)))))

(defun toolbox-tramp--path-for-buffer (path container)
  "For file at PATH format a find file string to open it inside CONTAINER."
  (let* ((toolbox . ((concat "/toolbox:" container ":")))
	 (full-path . ((expand-file-name path)))
	 (localised-path . ((if (file-remote-p full-path)
				(file-remote-p full-path 'localname)
			      full-path))))
	 (concat toolbox localised-path)))

(defun toolbox-tramp-reopen-file-in-toolbox (buffer container)
  "Reopen a BUFFER inside a toolbox CONTAINER.
This also allows for changing current container."
  (interactive (list
		(read-buffer "Buffer: " (current-buffer) t)
		(completing-read "Select container: " (toolbox-tramp-toolbox-containers))))
  (find-alternate-file (toolbox-tramp--path-for-buffer
			(buffer-file-name (get-buffer buffer))
			 container)))

;;;###autoload
(defun toolbox-tramp-login-args ()
  "Return the correct login args for the connection type."
    toolbox-tramp-podman-args)

;;;###autoload
(defconst toolbox-tramp-completion-function-alist
  '((toolbox-tramp-toolbox-containers-completion ""))
  "Default list of (FUNCTION FILE) pairs to be examined for toolbox method.")

;;;###autoload
(defconst toolbox-tramp-method "toolbox"
  "Method to connect toolbox containers.")

;;;###autoload
(defun toolbox-tramp-add-method ()
  "Add toolbox tramp method."
  (add-to-list 'tramp-methods
	       `(,toolbox-tramp-method
		 (tramp-login-program      ,toolbox-tramp-executable)
		 (tramp-login-args         ,(toolbox-tramp-login-args))
		 (tramp-remote-shell       ,tramp-default-remote-shell)
		 (tramp-remote-shell-login ("-l"))
		 (tramp-remote-shell-args  ("-i -c")))))

(defconst toolbox-tramp-default-prefix "fedora-toolbox-")

(defvar toolbox-tramp-default-container
  (when (eq system-type 'gnu)
      (with-temp-buffer
	(insert-file-contents
	 (if-let (file-exists-p "/run/host/etc/os-release") "/run/host/etc/os-release" "/etc/os-release"))
	(keep-lines "VERSION_ID" (point-min) (point-max))
	(concat toolbox-tramp-default-prefix (when (string-match "VERSION_ID=\\(.*\\)" (buffer-string))
					       (match-string 1 (buffer-string)))))))

(defvar toolbox-tramp-default-user
      (user-login-name))
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
;;; toolbox-tramp.el ends here
