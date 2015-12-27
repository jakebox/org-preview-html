;;; org-preview.el --- automatically use eww to preview current org-file when save

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
;; org-preview's code can be found here:
;;   http://github.com/lujun9972/org-preview

;;; Commentary:

;; org-preview is a little tool that use eww to preview current org-file when save automatically 

;; Quick start:

;; execute the following commands:
;; `org-preview-turn-on-preview-at-save'

;;; Code:
(require 'org)
(require 'eww)

(defvar org-preview-output-file-name "realtime-preview-result.html"
  "temporary file generated when preview")

(defun org-preview--select-or-create-buffer-window (buffer-or-name)
  "If any window in currect frame displaying BUFFER-OR-NAME ,then select the window, otherwise,create a new window to display it"
  (let ((buf (get-buffer-create buffer-or-name)))
	(unless (get-buffer-window buf)
	  (split-window)
	  (switch-to-buffer buf))
	(select-window (get-buffer-window buf))))

(defun org-preview--buffer-point(buffer-or-name &optional default-point)
  "Get the point position in specify buffer

If BUFFER-OR-NAME did not exist, return DEFAULT-POINT"
  (if (get-buffer buffer-or-name)
	  (with-current-buffer buffer-or-name
		(point))
	default-point))

(defun org-preview-convert (output-file-name)
  "Export current org-mode buffer to OUTPUT-FILE-NAME, and call `eww-open-file' to preview it"
  (let ((cb (current-buffer))
		(eww-point (org-preview--buffer-point "*eww*" 1)))
    (save-excursion
	  (org-preview--select-or-create-buffer-window "*eww*")
	  (with-current-buffer cb
		(org-export-to-file 'html output-file-name nil nil nil nil nil #'eww-open-file))
	  (goto-char eww-point))
    (org-preview--select-or-create-buffer-window cb)))

(defun org-preview ()
  "Export current org-mode buffer to `org-preview-output-file-name', and call `eww-open-file' to preview it"
  (interactive)
  (org-preview-convert org-preview-output-file-name))

(defun org-preview-turn-on-preview-at-save ()
  "turn on automatically preview current org-file when save"
  (interactive)
  (add-hook 'after-save-hook #'org-preview nil t))

(defun org-preview-turn-off-preview-at-save ()
  "turn off automatically preview current org-file when save"
  (interactive)
  (remove-hook 'after-save-hook #'org-preview t))

(provide 'org-preview)

;;; org-preview.el ends here
