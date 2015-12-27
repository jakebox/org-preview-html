;;; org-eww.el --- automatically use eww to preview current org-file when save

;; Copyright (C) 2004-2015 Free Software Foundation, Inc.

;; Author: DarkSun <lujun9972@gmail.com>
;; Created: 2015-12-27
;; Version: 0.1
;; Keywords: convenience, eww, org
;; Package-Requires: ((org "8.0") (eww))

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
;; `org-eww-turn-on-preview-at-save'

;;; Code:
(require 'org)
(require 'eww)

(defvar org-eww-output-file-name "realtime-preview-result.html"
  "temporary file generated when preview")

(defun org-eww--select-or-create-buffer-window (buffer-or-name)
  "If any window in currect frame displaying BUFFER-OR-NAME ,then select the window, otherwise,create a new window to display it"
  (let ((buf (get-buffer-create buffer-or-name)))
	(unless (get-buffer-window buf)
	  (split-window)
	  (switch-to-buffer buf))
	(select-window (get-buffer-window buf))))

(defun org-eww--buffer-point(buffer-or-name &optional default-point)
  "Get the point position in specify buffer

If BUFFER-OR-NAME did not exist, return DEFAULT-POINT"
  (if (get-buffer buffer-or-name)
	  (with-current-buffer buffer-or-name
		(point))
	default-point))

(defun org-eww-convert (output-file-name)
  "Export current org-mode buffer to OUTPUT-FILE-NAME, and call `eww-open-file' to preview it"
  (let ((cb (current-buffer))
		(eww-point (org-eww--buffer-point "*eww*" 1)))
    (save-excursion
	  (org-eww--select-or-create-buffer-window "*eww*")
	  (with-current-buffer cb
		(org-export-to-file 'html output-file-name nil nil nil nil nil #'eww-open-file))
	  (goto-char eww-point))
    (org-eww--select-or-create-buffer-window cb)))

(defun org-eww ()
  "Export current org-mode buffer to `org-eww-output-file-name', and call `eww-open-file' to preview it"
  (interactive)
  (org-eww-convert org-eww-output-file-name))

(defun org-eww-turn-on-preview-at-save ()
  "turn on automatically preview current org-file when save"
  (interactive)
  (add-hook 'after-save-hook #'org-eww nil t))

(defun org-eww-turn-off-preview-at-save ()
  "turn off automatically preview current org-file when save"
  (interactive)
  (remove-hook 'after-save-hook #'org-eww t))

(provide 'org-eww)

;;; org-eww.el ends here
