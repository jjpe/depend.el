;;; depend.el --- A system to manage dependencies that need to be downloaded

;;; Commentary:

;;; Code:

(require 'dash) ;; threading macros

(defvar depend/semver "0.3.0"
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
  "Log formatted MSG (including any ARGS) to the `depend/buffer-name' buffer."
  (with-current-buffer (get-buffer-create depend/buffer-name)
    (goto-char (point-max))
    (insert-string (apply #'format msg args))
    (insert-string "\n")))

(defun depend/download (url file-path)
  "Download a resource from URL to FILE-PATH."
  (let* ((stem (concat depend/root-directory "bin/download-" depend/bin-semver))
         (bin (if depend/debug
                  (concat stem "-" depend/os "-dbg")
                (concat stem "-" depend/os)))
         (proc-name depend/buffer-name))
    (call-process bin nil (get-buffer-create "*depend*") t
                  "--from" url
                  "--to" file-path)))

(defun depend/make-executable (file-path)
  "Make FILE-PATH executable."
  (let* ((proc-name depend/buffer-name))
    (unless (process-live-p (get-process proc-name))
      (call-process "chmod" nil (get-buffer-create "*depend*") t
                    "ug+x"
                    file-path)
      (depend/log "Made %s executable" file-path))))

(defun depend/extract-zip (zip-file-name target-dir-path)
  "Extract an archive, located at ZIP-FILE-NAME, to a TARGET-DIR-PATH.
If the TARGET-DIR-PATH already exists, skip the extraction."
  (let ((buffer (get-buffer-create depend/buffer-name))
        (command (concat "unzip " zip-file-name " -d " target-dir-path)))
    (with-current-buffer buffer
      (goto-char (point-max))
      (if (file-exists-p target-dir-path)
          (depend/log "using cached dir @ %s" target-dir-path)
        (progn
          (call-process "unzip" nil (get-buffer-create "*depend*") t
                        zip-file-name "-d" target-dir-path)
          (depend/log "extracted dir @ %s" target-dir-path))))))

(provide 'depend)
;;; depend.el ends here
