;;; md-capture.el --- Simple Markdown capture system -*- lexical-binding: t; -*-

;; Author: Mike Hall
;; Version: 0.6
;; Package-Requires: ((emacs "27.1"))
;; Keywords: markdown, convenience, capture
;; URL: https://github.com/pdxmph/md-capture

;;; Commentary:
;;
;; A lightweight capture tool for Markdown files, inspired by org-capture.
;; Creates a transient buffer for writing a timestamped post and saves it to a
;; target Markdown file.

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

(defvar md-capture--destination nil
  "Internal variable to store the destination file during a capture session.")

(define-minor-mode md-capture-mode
  "Minor mode for md-capture buffers."
  :lighter " >> Capture"
  :keymap (let ((map (make-sparse-keymap)))
            (define-key map (kbd "C-c C-c") #'md-capture-finalize)
            (define-key map (kbd "C-c C-k") #'md-capture-abort)
            map))

(defun md-capture ()
  "Start a Markdown capture session in an indirect buffer."
  (interactive)
  (let* ((targets (cond
                   (md-capture-targets md-capture-targets)
                   (md-capture-dir (directory-files md-capture-dir t "\\.md$"))
                   (t (user-error "No md-capture targets or directory defined."))))
         (target (completing-read "Capture to: " targets nil t))
         (title (read-string "Post title: "))
         (date (format-time-string "%Y-%m-%d %a %H:%M"))
         (entry-header (format "\n\n## [%s] %s\n\n" date title))
         (capture-buffer (generate-new-buffer "*md-capture*")))
    (split-window-below -12)
    (other-window 1)
    (switch-to-buffer capture-buffer)
    (insert entry-header)
    (markdown-mode)
    (md-capture-mode 1)
    (setq md-capture--destination target)
    (message "Write your post. C-c C-c to save, C-c C-k to cancel.")))

(defun md-capture-finalize ()
  "Save the capture buffer's contents to the target file."
  (interactive)
  (let ((content (string-trim-right (buffer-string)))
        (dest md-capture--destination))
    (with-current-buffer (find-file-noselect dest)
      (goto-char (point-min))
      ;; Ensure first line is a level-1 title
      (unless (looking-at "^# ")
        (insert (format "# %s\n\n" (file-name-base dest))))
      (goto-char (point-min))
      (if (re-search-forward "^## " nil t)
          (progn
            (beginning-of-line)
            (message "Inserting above first ## at position %d" (point))
            ;; Clean blank lines above
            (while (and (not (bobp))
                        (save-excursion (forward-line -1) (looking-at "^\\s-*$")))
              (forward-line -1)
              (delete-blank-lines))
            (insert "\n")
            (insert content "\n\n"))
        (goto-char (point-max))
        (message "No ## found; appending to end at position %d" (point))
        (insert "\n" content "\n\n"))
      (save-buffer))
    (md-capture--cleanup)
    (message "Capture saved to %s." md-capture--destination)))

(defun md-capture-abort ()
  "Cancel the current capture session."
  (interactive)
  (when (y-or-n-p "Abort this capture? ")
    (md-capture--cleanup)
    (message "Capture aborted.")))

(defun md-capture--cleanup ()
  "Close and kill the capture buffer."
  (let ((buf (current-buffer)))
    (kill-buffer buf)
    (delete-window)))

(provide 'md-capture)
;;; md-capture.el ends here
