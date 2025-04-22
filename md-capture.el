;;; md-capture.el --- Simple Markdown capture system -*- lexical-binding: t; -*-

;; Author: Mike Hall and a Robot
;; Version: 0.1
;; Package-Requires: ((emacs "27.1"))
;; Keywords: markdown, convenience, capture
;; URL: https://github.com/you/md-capture

;;; Commentary:
;;
;; A lightweight capture system for Markdown files, inspired by org-capture.
;; Inserts date-stamped level-2 headings in a selected Markdown file.
;; Files can be selected from a list or discovered from a directory.

;;; Code:

(defgroup md-capture nil
  "Markdown capture system."
  :group 'convenience
  :prefix "md-capture-")

(defcustom md-capture-targets nil
  "List of Markdown files to use as capture targets."
  :type '(repeat file))

(defcustom md-capture-dir nil
  "Directory from which to use all *.md files as capture targets."
  :type 'directory)

(defvar-local md-capture--destination nil
  "Internal variable to store the destination file during a capture session.")

(define-minor-mode md-capture-mode
  "Minor mode for md-capture buffers."
  :lighter " ✍️Capture"
  :keymap (let ((map (make-sparse-keymap)))
            (define-key map (kbd "C-c C-c") #'md-capture-finalize)
            (define-key map (kbd "C-c C-k") #'md-capture-abort)
            map))

(defun md-capture ()
  "Start a Markdown capture session."
  (interactive)
  (let* ((targets (cond
                   (md-capture-targets md-capture-targets)
                   (md-capture-dir (directory-files md-capture-dir t "\\.md$"))
                   (t (user-error "No md-capture targets or directory defined."))))
         (target (completing-read "Capture to: " targets nil t))
         (title (read-string "Post title: "))
         (date (format-time-string "%Y-%m-%d"))
         (entry (format "## [%s] %s\n\n" date title)))
    (with-current-buffer (find-file-noselect target)
      ;; Ensure first line is a level-1 title
      (goto-char (point-min))
      (unless (looking-at "^# ")
        (insert (format "# %s\n\n" (file-name-base target))))
      (goto-char (point-min))
      (forward-line 1)
      (insert entry)
      (save-buffer)
      ;; Transient capture window
      (let ((buf (current-buffer)))
        (split-window-below -12)
        (other-window 1)
        (switch-to-buffer buf)
        (goto-char (point-min))
        (re-search-forward (regexp-quote entry))
        (md-capture-mode 1)
        (setq-local md-capture--destination target)))))

(defun md-capture-finalize ()
  "Finalize and save the capture."
  (interactive)
  (save-buffer)
  (md-capture--cleanup)
  (message "Capture saved."))

(defun md-capture-abort ()
  "Abort the capture session."
  (interactive)
  (when (y-or-n-p "Abort this capture? ")
    (revert-buffer :ignore-auto :noconfirm)
    (md-capture--cleanup)
    (message "Capture aborted.")))

(defun md-capture--cleanup ()
  "Clean up the capture window and buffer."
  (let ((buf (current-buffer)))
    (delete-window)
    (kill-buffer buf)))

(provide 'md-capture)
;;; md-capture.el ends here
