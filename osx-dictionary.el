;;; osx-dictionary.el --- use Mac OSX's builtin dictionary  -*- lexical-binding: t; -*-

;; Copyright (C) 2013-2016  Leo Liu

;; Author: Leo Liu <sdl.web@gmail.com>
;; Version: 1.1
;; Package-Requires: ((emacs "24.4"))
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

;; Place osxdict in exec-path and M-x osx-dictionary RET.

;;; Code:

(eval-when-compile (require 'cl-lib))

(defcustom osx-dictionary-osxdict-program "osxdict"
  "Name of the `osxdict' program."
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
                       (cl-loop repeat 10
                                while (not (re-search-backward
                                            comint-prompt-regexp nil t))
                                do (sit-for 0.1))
                       (goto-char (point-max)))
                     proc))))
      (with-temp-buffer
        (comint-redirect-send-command-to-process
         phrase (current-buffer) proc nil t)
        (cl-loop repeat 6
                 while (not (buffer-local-value 'comint-redirect-completed
                                                buffer))
                 do (sit-for 0.05))
        (goto-char (point-min))
        (unless (looking-at-p "error: ")
          (buffer-string))))))

(defun osx-dictionary-break-longlines ()
  (let ((width (max fill-column 70)))
    (goto-char (point-min))
    (while (not (eobp))
      (end-of-line)
      (when (> (current-column) width)
        (let ((adaptive-fill-regexp (concat "^ *[0-9]+ \\|" adaptive-fill-regexp)))
          (fill-region (line-beginning-position) (line-end-position))))
      (forward-line 1))
    (delete-trailing-whitespace)))

(defvar osx-dictionary-heading-re
  (eval-when-compile
    (regexp-opt '("DERIVATIVES" "PHRASES" "PHRASAL VERBS" "ORIGIN"))))

(defun osx-dictionary-format-definition ()
  (goto-char (point-min))
  (while (not (eobp))
    (pcase (following-char)
      ((or ?\[ ?\() (forward-sexp 1))
      (?\x2018 (skip-chars-forward "^\x2019"))
      (?\x25b6 (insert "\n\n") (forward-char 1))
      (?\x2022 (insert "\n\n  ") (forward-char 1))
      ((and (or ?1 ?2 ?3 ?4 ?5 ?6 ?7 ?8 ?9) (guard (= (preceding-char) ?\s)))
       (let ((start (point)))
         (skip-chars-forward "0-9")
         (and (= (following-char) ?\s)
              ;; Assume number of ordered items should not exceed 20.
              ;; A naive way to reduce false positives.
              (< (string-to-number (buffer-substring start (point))) 20)
              (save-excursion (goto-char start) (insert "\n\n")))))
      ((guard (let (case-fold-search)
                (looking-at osx-dictionary-heading-re)))
       (save-match-data (insert "\n\n"))
       (goto-char (match-end 0))
       (delete-horizontal-space)
       (newline))
      (_ (forward-char 1))))
  (osx-dictionary-break-longlines))

(defun osx-dictionary-fontify ()
  (let (case-fold-search)
    (goto-char (point-min))
    (let ((start (point))
          (end (progn (forward-word) (point))))
      (make-text-button
       start end 'follow-link t
       'help-echo (format "Visit dict://%s" (buffer-substring start end))
       'action (lambda (b)
                 (osx-dictionary-open
                  (buffer-substring-no-properties
                   (button-start b) (button-end b))))))
    (while (re-search-forward osx-dictionary-heading-re nil t)
      (add-face-text-property (match-beginning 0) (match-end 0) 'bold))
    (goto-char (point-min))
    (while (re-search-forward "|[^ \t\n][^|]*|" nil t)
      (add-face-text-property (match-beginning 0) (match-end 0)
                              font-lock-keyword-face))
    (goto-char (point-min))
    (while (re-search-forward "\\[\\(.*?\\)\\]" nil t)
      (add-face-text-property (match-beginning 1) (match-end 1) 'italic))))

(eval-when-compile (require 'ispell)) ; for ispell-alternate-dictionary
(defvar osx-dictionary-completion-table
  (let ((file (eval-when-compile ispell-alternate-dictionary)))
    (when (and file (file-readable-p file))
      (completion-table-with-cache
       (lambda (s)
         (when (> (length s) 0)
           (with-current-buffer (get-buffer-create " *words*")
             (when (zerop (buffer-size))
               (insert-file-contents file))
             (goto-char (point-min))
             (cl-loop while (search-forward s nil t) collect
                      (progn (end-of-line)
                             (buffer-substring (line-beginning-position)
                                               (line-end-position)))))))
       t))))

(defun osx-dictionary-read-word ()
  (let* ((word (current-word nil t))
         (prompt (format (if word "Phrase (default `%s'): " "Phrase: ")
                         word)))
    (if osx-dictionary-completion-table
        ;; `initials' completion can be slow, test example: "ispe"
        (let ((completion-styles (or (remove 'initials completion-styles)
                                     '(basic))))
          (completing-read prompt osx-dictionary-completion-table
                           nil nil nil nil word))
      (read-string prompt nil nil word))))

;;;###autoload
(defun osx-dictionary-open (phrase)
  "Look up PHRASE in Dictionary.app."
  (interactive (list (osx-dictionary-read-word)))
  (let ((uri (format "dict://%s" phrase)))
    (if (fboundp 'do-applescript)
        (do-applescript
         (string-to-multibyte
          (format "tell application \"Finder\" to open location %S"
                  (url-encode-url uri))))
      (call-process "open" nil nil nil uri))))

(defun osx-dictionary-speak (text)
  (interactive (list (osx-dictionary-read-word)))
  (if (fboundp 'do-applescript)
      (do-applescript
       (string-to-multibyte
        (format "say %S without waiting until completion" text)))
    (call-process "say" nil 0 nil text)))

;;;###autoload
(defun osx-dictionary (phrase &optional raw)
  "Look up PHRASE in the builtin dictionary."
  (interactive (list (osx-dictionary-read-word) current-prefix-arg))
  (let ((text (or (osx-dictionary-get-definition phrase)
                  (user-error "No definition found for `%s'" phrase))))
    (osx-dictionary-speak phrase)
    ;; Put the formatted text in the help-buffer.
    (help-setup-xref (list #'osx-dictionary phrase raw)
                     (called-interactively-p 'interactive))
    (with-help-window (help-buffer)
      (with-current-buffer (help-buffer)
        (insert text)
        (unless raw
          (osx-dictionary-format-definition)
          (osx-dictionary-fontify))))))

(provide 'osx-dictionary)
;;; osx-dictionary.el ends here
