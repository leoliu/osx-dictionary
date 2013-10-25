;;; t.el --- tests for osx-dictionary                -*- lexical-binding: t; -*-

;; Copyright (C) 2013  Leo Liu

;; Author: Leo Liu <sdl.web@gmail.com>
;; Keywords: maint

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

;;; Code:

(require 'osx-dictionary)

(setq sentence-end-double-space nil)
(setq osx-dictionary-osxdict-program (expand-file-name "../osxdict"))
(when (fboundp 'osx-ds-get-definition)
  (fmakunbound 'osx-ds-get-definition))

(ert-deftest test-osx-dictionary-format-definition ()
  (let ((files (directory-files default-directory t
                                "\\`[0-9][0-9].*\\.txt\\'")))
    (dolist (f files)
      (let ((phrase (substring (file-name-sans-extension
                                (file-name-nondirectory f)) 2)))
        (message "---------------->> %s" phrase)
        (should (equal (substring-no-properties
                        (osx-dictionary-format-definition
                         (osx-dictionary-get-definition phrase)))
                       (with-temp-buffer
                         (insert-file-contents f)
                         (buffer-string))))))))

(provide 't)
;;; t.el ends here
