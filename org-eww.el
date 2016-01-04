;;; org-eww.el --- automatically use eww to preview current org-file when save

;; Copyright (C) 2004-2015 Free Software Foundation, Inc.

;; Author: DarkSun <lujun9972@gmail.com>
;; Created: 2015-12-27
;; Version: 0.1
;; Keywords: convenience, eww, org
;; Package-Requires: ((org "8.0") (emacs "24.4"))
;; URL: https://github.com/lujun9972/org-eww

;; This file is NOT part of GNU Emacs.

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

;;; Source code
;;
;; org-eww's code can be found here:
;;   http://github.com/lujun9972/org-eww

;;; Commentary:

;; org-eww is a little tool that use eww to preview current org-file when save automatically 

;; Quick start:

;; execute the following commands:
;; `org-eww/turn-on-preview-at-save'

;;; Code:
(require 'org)
(require 'eww)

(defun org-eww/convert (output-file-name)
  "Export current org-mode buffer to OUTPUT-FILE-NAME, and call `eww-open-file' to preview it"
  (let ((cb (current-buffer)))
    (save-excursion
      (with-selected-window (display-buffer (get-buffer-create "*eww*"))
        (let ((eww-point (point))
              (eww-window-start (window-start)))
          (with-current-buffer cb
            (org-export-to-file 'html output-file-name nil nil nil nil nil #'eww-open-file))
          (goto-char eww-point)
          (set-window-start nil eww-window-start))))))

;;;###autoload
(defun org-eww ()
  "Export current org-mode buffer to a temp file and call `eww-open-file' to preview it"
  (interactive)
  (org-eww/convert (make-temp-file (file-name-base buffer-file-name) nil ".html")))

;;;###autoload
(defun org-eww/turn-on-preview-at-save ()
  "turn on automatically preview current org-file when save"
  (interactive)
  (add-hook 'after-save-hook #'org-eww nil t))

;;;###autoload
(defun org-eww/turn-off-preview-at-save ()
  "turn off automatically preview current org-file when save"
  (interactive)
  (remove-hook 'after-save-hook #'org-eww t))

(provide 'org-eww)

;;; org-eww.el ends here
