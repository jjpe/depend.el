;;; depend.el --- A system to manage dependencies that need to be downloaded

;;; Commentary:

;;; Code:

(require 'dash) ;; threading macros

(defvar depend/semver "0.2.0"
  "The semantic version of this depend.el release.")

(defvar depend/bin-semver "0.1.0"
  "The semantic version of the `bin/download-*' executables that will be used.")

(defvar depend/buffer-name "*depend*")

(defvar depend/root-directory (file-name-directory load-file-name)
  "The depend.el root directory.")

(defvar load-path nil)
(add-to-list 'load-path (directory-file-name depend/root-directory))

(defvar depend/os
  (pcase system-type
    ('darwin       "osx")
    ('gnu/linux    "linux-x86-64")
    ;; TODO: Windows support
    (_ (error "Operating system '%s' is not supported" system-type)))
  "A tag representing the current operating system.")

(defvar depend/debug t
  "Set to t if this code should run in debug mode.  Set to nil otherwise.")

(defun depend/log (msg &rest args)
  "log MSG to the `depend/buffer-name' buffer."
  (with-current-buffer depend/buffer-name
    (goto-char (point-max))
    (insert-string (apply #'format msg args))
    (insert-string "\n")))

(defun depend/download (url file-path)
  "Download a resource from URL to FILE-PATH."
  (let* ((bin (if depend/debug
                  (concat "bin/download-" depend/bin-semver "-" depend/os "-dbg ")
                (concat "bin/download-" depend/bin-semver "-" depend/os " ")))
         (command (concat depend/root-directory bin
                          "--from " url " "
                          "--to " file-path " "
                          ;; TODO: This is poor man's error handling
                          ;;            and should be improved:
                          "|| echo " file-path " already exists"))
         (proc-name depend/buffer-name)
         (buffer-name proc-name))
    (unless (process-live-p (get-process proc-name))
      (start-process-shell-command proc-name buffer-name command))))

(defun depend/make-executable (file-path)
  "Make FILE-PATH executable."
  (let* ((command (concat "chmod ug+x " file-path " "
                          "&& echo \"Made " file-path " executable\" "))
         (proc-name depend/buffer-name)
         (buffer-name proc-name))
    (unless (process-live-p (get-process proc-name))
      (start-process-shell-command proc-name buffer-name command))))

(defun depend/extract-zip (zip-file-name target-dir-path)
  "Extract an archive, located at ZIP-FILE-NAME, to a TARGET-DIR-PATH.
If the TARGET-DIR-PATH already exists, skip the extraction."
  (let* ((buffer-name depend/buffer-name)
         (buffer (get-buffer-create buffer-name)))
    (with-current-buffer buffer
      (goto-char (point-max))
      (if (file-exists-p target-dir-path)
          (depend/log "using cached dir @ %s" target-dir-path)
        (progn
          (shell-command (concat "unzip " zip-file-name " -d " target-dir-path)
                         buffer buffer)
          (depend/log "extracted dir @ %s" target-dir-path))))))

(provide 'depend)
;;; depend.el ends here
