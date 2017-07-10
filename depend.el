;;; depend.el --- A system to manage dependencies that need to be downloaded

;;; Commentary:

(require 'dash) ;; threading macros

(defvar depend/semver "0.1.0"
  "The semantic version of this depend.el release.")

(defvar depend/root-directory (file-name-directory load-file-name)
  "The depend.el root directory.")

(defvar depend/os
  (pcase system-type
    ('darwin       "osx")
    ('gnu/linux    "linux-x86-64")
    ;; TODO: Windows support
    (_ (error "Operating system '%s' is not supported" system-type)))
  "A tag representing the current operating system.")

(defvar depend/debug t
  "Set to t if this code should run in debug mode.  Set to nil otherwise.")

(defun depend/download (url file-path)
  "Download a resource from URL to FILE-PATH."
  (let* ((bin (if depend/debug
                  (concat "bin/download-" depend/semver "-" depend/os "-dbg ")
                (concat "bin/download-" depend/semver "-" depend/os " ")))
         (command (concat bin
                          "--from " url " "
                          "--to " file-path " "
                          ;; TODO: This is poor man's error handling
                          ;;            and should be improved:
                          "|| echo " file-path " already exists "))
         (proc-name "*depend*")
         (buffer-name proc-name))
    (unless (process-live-p (get-process proc-name))
      (start-process-shell-command proc-name buffer-name command))))

(defun depend/make-executable (file-path)
  "Make FILE-PATH executable."
  (let* ((command (concat "chmod ug+x " file-path))
         (proc-name "*depend*")
         (buffer-name proc-name))
    (unless (process-live-p (get-process proc-name))
      (start-process-shell-command proc-name buffer-name command))))

;; (depend/download
;;  "https://github.com/jjpe/amplify/releases/download/0.14.0/amplify-0.14.0-osx-dbg"
;;  (concat depend/root-directory "bin/amplify-0.14.0-osx-dbg"))

;; (depend/make-executable
;;  (concat depend/root-directory "bin/amplify-0.14.0-osx-dbg"))

;;; Code:

(provide 'depend)
;;; depend.el ends here
