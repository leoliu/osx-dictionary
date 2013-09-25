;;; osx-dictionary.el --- use Mac OSX's builtin dictionary  -*- lexical-binding: t; -*-

;; Copyright (C) 2013  Leo Liu

;; Author: Leo Liu <sdl.web@gmail.com>
;; Version: 1.0
;; Keywords: tools
;; Created: 2013-09-23

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

;; Place osxdict.py in exec-path and M-x osx-dictionary RET.

;;; Code:

(eval-when-compile (require 'cl))

(defcustom osx-dictionary-osxdict-program "osxdict.py"
  "Name of the `osxdict.py' program."
  :type 'string
  :group 'tools)

(defun osx-dictionary-get-definition (phrase)
  (if (fboundp 'osx-ds-get-definition)
      (osx-ds-get-definition phrase)
    (eval-and-compile (require 'comint))
    (let* ((buffer (get-buffer-create " *osxdict*"))
           (proc (if (comint-check-proc buffer)
                     (get-buffer-process buffer)
                   (with-current-buffer buffer
                     ;; Erase, or re-search-backward will succeed falsely.
                     (let ((inhibit-read-only t)) (erase-buffer)))
                   (let ((proc (comint-exec-1 "osxdict"
                                              buffer
                                              osx-dictionary-osxdict-program
                                              nil)))
                     (with-current-buffer (process-buffer proc)
                       (comint-mode)
                       (set-process-query-on-exit-flag proc nil)
                       (setq-local comint-use-prompt-regexp t)
                       (setq-local comint-prompt-regexp "^DICT> *")
                       (message "Waiting for osxdict to be ready")
                       (loop repeat 10
                             while (not (re-search-backward
                                         comint-prompt-regexp nil t))
                             do (sit-for 0.1))
                       (goto-char (point-max)))
                     proc))))
      (with-temp-buffer
        (comint-redirect-send-command-to-process
         phrase (current-buffer) proc nil t)
        (loop repeat 6
              while (not (buffer-local-value 'comint-redirect-completed
                                             buffer))
              do (sit-for 0.05))
        (goto-char (point-min))
        (when (not (looking-at-p "error: "))
          (buffer-string))))))

(defun osx-dictionary-break-longlines ()
  (let ((width (max fill-column 70)))
    (goto-char (point-min))
    (forward-line 1)
    (loop repeat 3
          while (or (eq (char-after) ?|)
                    (save-excursion (forward-word) (eolp)))
          do (forward-line 1))
    (fill-region (point-min) (point))
    (while (not (eobp))
      (end-of-line)
      (when (> (current-column) width)
        (fill-region (line-beginning-position) (line-end-position)))
      (forward-line 1))
    (delete-trailing-whitespace)))

(defun osx-dictionary-fontify (re face &optional sub fn anchor)
  (goto-char (point-min))
  (when (or (not anchor) (re-search-forward anchor nil t))
    (let ((sub (or sub 0)))
      (while (re-search-forward re nil t)
        (when face
          (put-text-property (match-beginning sub) (match-end sub)
                             'face face))
        (when fn
          (funcall fn (match-beginning 0) (match-end 0)))))))

(defun osx-dictionary-open (phrase)
  (let ((uri (format "dict://%s" phrase)))
    (if (fboundp 'do-applescript)
        (do-applescript
         (string-to-multibyte
          (format "tell application \"Finder\" to open location %S"
                  (url-encode-url uri))))
      (call-process "open" nil nil nil uri))))

(defun osx-dictionary-format-definition (text)
  (with-temp-buffer
    (insert text)
    (osx-dictionary-fontify
     "\\`\\(.*?\\)[ 0-9]*$"             ; skip superscripts
     nil nil
     (lambda (beg _end)
       (let ((end (match-end 1))
             (action (lambda (b)
                       (osx-dictionary-open
                        (buffer-substring-no-properties
                         (button-start b) (button-end b))))))
         (make-text-button
          beg end
          'follow-link t
          'help-echo (format "Visit dict://%s" (buffer-substring beg end))
          'mouse-action action
          'action action)
         ;; Avoid infinite loop.
         (forward-line 1))))
    (osx-dictionary-fontify "|[^|\n]+|" font-lock-keyword-face)
    (osx-dictionary-fontify "^(\n\\([^)]+\\)\n) ?$" 'bold 1 #'fill-region)
    (let ((case-fold-search nil))
      (osx-dictionary-fontify "^[A-Z]\\{3,\\}" 'italic))
    (osx-dictionary-fontify
     "^\\([0-9]+ \\)\n\\(?:.\\|\n\\)+?\\([:.;]\\) *$"
     nil nil
     (lambda (beg end)
       (if (and (eq (char-after (match-beginning 2)) ?.)
                (progn
                  (goto-char (match-beginning 2))
                  (save-match-data (let ((case-fold-search nil))
                                     (looking-back "\\bBrit\\|\\bAmer")))))
           (progn
             (end-of-line)
             (delete-char 1)
             (just-one-space)
             (goto-char (match-beginning 0)))
         (let ((end (copy-marker end t)))
           (when (and (equal (match-string 2) ":")
                      (or (eq (char-after (1+ end)) ?\()
                          (looking-at-p "\nSee also")))
             (move-marker end (line-end-position 2)))
           (goto-char (match-beginning 1))
           (forward-line 1)
           (insert (make-string (length (match-string 1)) ?\s))
           (goto-char end)
           (fill-region beg end)
           (insert "\n")))))
    (osx-dictionary-fontify "\\(?:.\\|\n\\)+?\\(\n, *\\|\\'\\)"
                            nil nil
                            (lambda (beg _end)
                              (replace-match "" nil nil nil 1)
                              (fill-region beg (point)))
                            "DERIVATIVES")
    ;; Ensure one blank line before DERIVATIVES.
    (osx-dictionary-fontify "DERIVATIVES" nil nil
                            (lambda (_beg _end)
                              (forward-line -1)
                              (unless (looking-at-p "^[ \t]*$")
                                (end-of-line)
                                (insert "\n"))
                              (goto-char (point-max))))
    (osx-dictionary-break-longlines)
    (buffer-string)))

(eval-when-compile (require 'ispell)) ; for ispell-alternate-dictionary
(defvar osx-dictionary-completion-table
  (let ((file (eval-when-compile ispell-alternate-dictionary))
        (cache))
    (when (and file (file-readable-p file))
      (completion-table-dynamic
       (lambda (s)
         (when (> (length s) 0)
           (unless (equal (car cache) s)
             (let ((ws (with-current-buffer (get-buffer-create " *words*")
                         (when (zerop (buffer-size))
                           (insert-file-contents file))
                         (goto-char (point-min))
                         (loop while (search-forward s nil t)
                               collect (prog1
                                           (buffer-substring (line-beginning-position)
                                                             (line-end-position))
                                         (end-of-line))))))
               (setq cache (cons s ws))))
           (cdr cache)))))))

(defun osx-dictionary-read-word ()
  (let* ((word (current-word nil t))
         (prompt (format (if word "Phrase (default `%s'): " "Phrase: ")
                         word)))
    (if osx-dictionary-completion-table
        (completing-read prompt osx-dictionary-completion-table
                         nil nil nil nil word)
      (read-string prompt nil nil word))))

;;;###autoload
(defun osx-dictionary (phrase &optional raw)
  "Look up PHRASE in the builtin dictionary."
  (interactive (list (osx-dictionary-read-word) current-prefix-arg))
  (let ((text (funcall (if raw #'identity #'osx-dictionary-format-definition)
                       (or (osx-dictionary-get-definition phrase)
                           (user-error "No definition found for `%s'" phrase)))))
    ;; Put the formatted text in the help-buffer.
    (help-setup-xref (list #'osx-dictionary phrase raw)
                     (called-interactively-p 'interactive))
    (with-help-window (help-buffer)
      (with-current-buffer standard-output
        (insert text)))))

(provide 'osx-dictionary)
;;; osx-dictionary.el ends here
