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
