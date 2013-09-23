;;; osx-dictionary.el --- use Mac OSX's builtin dictionary  -*- lexical-binding: t; -*-

;; Copyright (C) 2013  Leo Liu

;; Author: Leo Liu <sdl.web@gmail.com>
;; Version: 0.6
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

(defun osx-dictionary-get-definition (phrase)
  (if (fboundp 'osx-ds-get-definition)
      (osx-ds-get-definition phrase)
    (eval-and-compile (require 'comint))
    (let* ((buffer (get-buffer-create " *osxdict*"))
           (proc (if (comint-check-proc buffer)
                     (get-buffer-process buffer)
                   (let ((proc (comint-exec-1 "osxdict" buffer "osxdict.py" nil)))
                     (with-current-buffer (process-buffer proc)
                       (comint-mode)
                       (set-process-query-on-exit-flag proc nil)
                       (setq-local comint-use-prompt-regexp t)
                       (setq-local comint-prompt-regexp "^DICT> *")
                       (message "Waiting for osxdict to be ready")
                       (loop repeat 10
                             while (not (re-search-forward
                                         comint-prompt-regexp nil t))
                             do (sit-for 0.1)))
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
  (let ((inhibit-read-only t)
        (width (max fill-column 70)))
    (goto-char (point-min))
    (forward-line 3)
    (when (looking-at-p "[^ \t]+$")
      (forward-line 1))
    (fill-region (point-min) (point))
    (while (not (eobp))
      (end-of-line)
      (when (> (current-column) width)
        (fill-region (line-beginning-position) (line-end-position)))
      (forward-line 1))
    (delete-trailing-whitespace)))

(defun osx-dictionary-fontify (re face &optional sub fn)
  (goto-char (point-min))
  (let ((inhibit-read-only t)
        (sub (or sub 0)))
    (while (re-search-forward re nil t)
      (when face
        (put-text-property (match-beginning sub) (match-end sub) 'face face))
      (when fn
        (funcall fn (match-beginning 0) (match-end 0))))))

(defun osx-dictionary-open (phrase)
  (let ((uri (format "dict://%s" phrase)))
    (if (fboundp 'do-applescript)
        (do-applescript
         (string-to-multibyte
          (format "tell application \"Finder\" to open location %s"
                  (prin1-to-string (url-encode-url uri)))))
      (call-process "open" nil nil nil uri))))

;;;###autoload
(defun osx-dictionary (phrase)
  "Look up PHRASE in the builtin dictionary."
  (interactive
   (let ((word (current-word nil t)))
     (list (read-string
            (format (if word "Phrase (default `%s'): " "Phrase: ") word)
            nil nil word))))
  (let ((definition (or (osx-dictionary-get-definition phrase)
                        (user-error "No definition found for `%s'" phrase))))
    (help-setup-xref (list #'osx-dictionary phrase)
                     (called-interactively-p 'interactive))
    (with-help-window (help-buffer)
      (princ definition))
    (with-current-buffer (help-buffer)
      (osx-dictionary-fontify
       "\\`\\(.*?\\)[ 0-9]*$"           ; skip superscripts
       nil nil
       (lambda (beg _end)
         (let ((end (match-end 1)))
           (make-text-button
            beg end
            'help-echo (format "Visit dict://%s" (buffer-substring beg end))
            'action (lambda (b)
                      (osx-dictionary-open
                       (buffer-substring-no-properties
                        (button-start b) (button-end b)))))
           ;; Avoid infinite loop.
           (forward-line 1))))
      (osx-dictionary-fontify "|[^|\n]+|" font-lock-keyword-face)
      (osx-dictionary-fontify "^(\n\\([^)]+\\)\n) ?$" 'bold 1 #'fill-region)
      (let ((case-fold-search nil))
        (osx-dictionary-fontify "^[A-Z]\\{3,\\}" 'italic))
      (osx-dictionary-fontify "^\\([0-9]+ \\)\n\\(?:.\\|\n\\)+?\\(?:: \\|[.;]\\)$"
                              nil nil
                              (lambda (beg end)
                                (let ((end (copy-marker end t)))
                                  (goto-char (match-beginning 1))
                                  (forward-line 1)
                                  (insert (make-string
                                           (length (match-string 1)) ?\s))
                                  (goto-char end)
                                  (fill-region beg end)
                                  (insert "\n"))))
      (osx-dictionary-break-longlines))))

;;; Examples
;; (osx-dictionary "call")
;; (osx-dictionary "fold")
;; (osx-dictionary "skip")
;; (osx-dictionary "break")
;; (osx-dictionary "attribute")
;; (osx-dictionary "while")

(provide 'osx-dictionary)
;;; osx-dictionary.el ends here
