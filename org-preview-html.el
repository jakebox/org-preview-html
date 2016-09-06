;;; org-preview-html.el --- automatically use eww to preview the current org file on save

;; Copyright (C) 2004-2016 DarkSun <lujun9972@gmail.com>

;; Author: DarkSun <lujun9972@gmail.com>
;; Created: 2015-12-27
;; Version: 0.2
;; Keywords: convenience, eww, org
;; Package-Requires: ((org "8.0") (emacs "24.4"))
;; URL: https://github.com/lujun9972/org-preview-html

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
;; org-preview-html's code can be found here:
;;   http://github.com/lujun9972/org-preview-html

;;; Commentary:

;; org-preview-html is a little tool that uses eww to automatically preview an
;; org-file on save.

;; Quick start:

;; Put this file under your load-path.
;; Enable the org-preview-html-mode in your org buffer:
;;   M-x org-preview-html-mode
;;
;;; Code:
(require 'org)
(require 'eww)

(defvar-local org-preview-html/htmlfilename nil
  "The temp exported html file")

(defun org-preview-html/preview ()
  "Export current org-mode buffer to a temp file and call `eww-open-file' to preview it."
  (let ((cb (current-buffer)))
    (save-excursion
      (with-selected-window (display-buffer (get-buffer-create "*eww*"))
        (let ((eww-point (point))
              (eww-window-start (window-start)))
          (with-current-buffer cb
            (org-export-to-file 'html org-preview-html/htmlfilename nil nil nil nil nil #'eww-open-file))
          (goto-char eww-point)
          (set-window-start nil eww-window-start))))))

(defun org-preview-html/turn-on-preview-on-save ()
  "Turn on automatic preview of the current org file on save."
  (progn
    (add-hook 'after-save-hook #'org-preview-html/preview nil t)
    (add-hook 'kill-buffer-hook #'org-preview-html/turn-off-preview-on-save nil t)
    ;; temp filename into a buffer local variable
    (setq org-preview-html/htmlfilename (concat buffer-file-name (make-temp-name "-") ".html"))
    ;; bogus file change to be able to save
    (insert " ")
    (delete-backward-char 1)
    ;; trigger creation of preview buffer
    (save-buffer)
    (message "Eww preview is on")))

(defun org-preview-html/turn-off-preview-on-save ()
  "Turn off automatic preview of the current org file on save."
  (progn
    (remove-hook 'after-save-hook #'org-preview-html/preview t)
    (remove-hook 'kill-buffer-hook #'org-preview-html/turn-off-preview-on-save t)
    (if (get-buffer "*eww*")
        (kill-buffer "*eww*"))
    (if (and (boundp 'org-preview-html/htmlfilename)
             org-preview-html/htmlfilename) (delete-file org-preview-html/htmlfilename))
    (message "Eww preview is off")))

;;;###autoload
(define-minor-mode org-preview-html-mode
  "Preview current org file in eww whenever you save it."
  :init-value nil
  :lighter " eww-prev"
  (if (and (boundp org-preview-html-mode)
           org-preview-html-mode)
      (org-preview-html/turn-on-preview-on-save)
    (org-preview-html/turn-off-preview-on-save)))

(provide 'org-preview-html)

;;; org-preview-html.el ends here
