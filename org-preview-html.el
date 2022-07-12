;;; org-preview-html.el --- Automatically preview org-exported HTML files within Emacs -*- lexical-binding: t; -*-

;; Copyright (C) 2021 Jake B <jakebox0@protonmail.com>

;; Author: Jake B <jakebox0@protonmail.com>
;; Original author of org-preview-html (until 2021-09): DarkSun <lujun9972@gmail.com>
;; Url: https://github.com/jakebox/org-preview-html
;; Keywords: Org, convenience, outlines
;; Version: 0.3
;; Package-Requires: ((emacs "25.1") (org "8.0"))

;; This file is NOT part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:
;; This minor mode provides a side-by-side preview of your org-exported HTML
;; files using the either the eww or xwidget browsers. The update frequency of
;; the preview can be configured to suit your preference.
;;
;; Quick start:
;; Put this file under your load path.
;; Enable the minor mode in an Org buffer:
;;   M-x org-preview-html-mode
;; Configure options with M-x customize-group org-preview-html
;;
;; Source code
;; org-preview-html's code can be found here:
;;   http://github.com/jakebox/org-preview-html

;;; Code:

;;;; Requirements
(require 'org)
(require 'xwidget)
(require 'eww)


(defgroup org-preview-html nil
  "Automatically preview org-exported HTML files within Emacs."
  :group 'org-mode
  :link '(url-link :tag "Homepage" "https://github.com/jakebox/org-preview-html/"))

(defcustom org-preview-html-refresh-configuration 'save
  "Specifies how often the HTML preview will be refreshed.
  
If `manual', update manually by running `org-preview-html-refresh'.
If `save', update on save (default).
If `export', update on manual export \(using `org-html-export-to-html').
If `timer', update preview on timer (`org-preview-html-timer-interval').
If `instant', update ASAP (may cause slowdowns)."
  :type '(choice
		  (symbol :tag "Update preview manually"   manual)
		  (symbol :tag "Update preview on save"    save)
		  (symbol :tag "Update preview on export"  export)
		  (symbol :tag "Update preview on a timer" timer)
		  (symbol :tag "Update preview instantly"  instant))
  :group 'org-preview-html)

(defcustom org-preview-html-timer-interval 2
  "Integer seconds to wait between exports when in 'timer mode."
  :type 'integer
  :group 'org-preview-html)

(defcustom org-preview-html-viewer 'eww
  "Which Emacs browser `org-preview-html-mode' will use.

If `eww', use eww browser (default).
If `xwidget', use xwidget browser."
  :type '(choice
		  (symbol :tag "Use eww"      eww)
		  (symbol :tag "Use xwidget"  xwidget))
  :group 'org-preview-html)

(define-obsolete-variable-alias 'org-preview-html/body-only 'org-preview-html-subtree-only "Version 0.3")

(defcustom org-preview-html-subtree-only nil
  "If non-nil, scope the preview to the current subtree."
  :type 'boolean
  :group 'org-preview-html)

(defcustom org-preview-html/body-only nil
  "Scope the preview to the body or include the entire document.
Obselete as of version 0.3, instead use `org-preview-html-subtree-only'."
  :type 'boolean
  :group 'org-preview-html)


;; Internal variables
(defvar org-preview-html--browser-buffer nil)
(defvar org-preview-html--previewed-buffer-name nil)
(defvar org-preview-html--refresh-timer nil)
(defvar-local org-preview-html--html-file nil)


;; https://emacs.stackexchange.com/questions/7116/pop-a-window-into-a-frame
(defun org-preview-html-pop-window-to-frame ()
  "Pop a window to a frame."
  (interactive)
  (let ((buffer (current-buffer)))
    (unless (one-window-p)
      (delete-window))
    (display-buffer-pop-up-frame buffer nil)))

;; Taken from frame.el Emacs 27.1, copied here for better version compatibility.
;; Without this here 27.1 required. With, 25.1.
(defun org-preview-html--previous-window-any-frame ()
  (select-window (previous-window (selected-window)
				  (> (minibuffer-depth) 0)
				  0))
  (select-frame-set-input-focus (selected-frame)))


(defun org-preview-html-refresh ()
  "Exports the org file to HTML and refreshes the preview."
  ;; Refresh the preview.
  (interactive)
  ;; WIP If in manual mode it doesn't matter what buffer is active, just export and refresh
  (cond
   ((eq org-preview-html-refresh-configuration 'manual) ;; if in manual mode
		 (pop-to-buffer org-preview-html--previewed-buffer-name nil t)
		 (org-preview-html--org-export-html)
		 (org-preview-html--reload-preview))
		((unless (or (eq (eq (get-buffer org-preview-html--previewed-buffer-name) ;; TODO JAKE WHAT IS THIS
                             ;; In timer and instant modes the visible buffer matters
							 (window-buffer (selected-window))) nil)
					 (or (let ((state org-preview-html-refresh-configuration))
						   (eq state 'timer) (eq state 'instant))))
		   (org-preview-html--org-export-html)
		   (org-preview-html--reload-preview)))))

(defun org-preview-html--org-export-html ()
  "Silently export org to HTML."
  (let ((standard-output 'ignore))
	(org-export-to-file 'html (substring org-preview-html--html-file 7)
	  nil org-preview-html-subtree-only nil nil nil nil)))

(defun org-preview-html--reload-preview ()
  "Reload preview."
  (save-selected-window
	(pop-to-buffer org-preview-html--browser-buffer)
	(cond ((eq org-preview-html-viewer 'xwidget) (xwidget-webkit-reload))
		  ((eq org-preview-html-viewer 'eww)
		   (with-selected-window (selected-window)
			 ;; This stuff is to keep eww window scrolled at same point
			 (let ((eww-point (point))
				   (eww-window-start (window-start)))
			   (eww-reload)
			   (goto-char eww-point)
			   (set-window-start nil eww-window-start)))))))

(defun org-preview-html--kill-preview-buffer ()
  "Kill the xwidget preview buffer and pop back to the previewed org buffer."
  ;; Only do these things if the preview is around
  (when (bound-and-true-p org-preview-html--browser-buffer)
    ;; If preview is visible we first delete the window, otherwise
	;; just kill the preview buffer
	(if (get-buffer-window org-preview-html--browser-buffer 'visible)
		(delete-window (get-buffer-window org-preview-html--browser-buffer)))
	(let ((kill-buffer-query-functions nil))
	  (kill-buffer org-preview-html--browser-buffer))
	(pop-to-buffer org-preview-html--previewed-buffer-name)))

(defun org-preview-html--run-with-timer ()
  "Configure timer to refresh preview for `timer' mode."
  (setq org-preview-html--refresh-timer
		(run-at-time 1 org-preview-html-timer-interval #'org-preview-html-refresh)))

(defun org-preview-html--config ()
  "Configure buffer for preview: add exit hooks; configure refresh hooks."
  (setq org-preview-html--previewed-buffer-name (buffer-name))
  (dolist (hook '(kill-buffer-hook kill-emacs-hook)) ;; Configure exit hooks
    (add-hook hook #'org-preview-html--stop-preview nil t))
  (let ((conf org-preview-html-refresh-configuration))
	(cond
	 ((eq conf 'manual))
	 ((eq conf 'save) ;; On save
	  (add-hook 'after-save-hook #'org-preview-html-refresh nil t))
	 ((eq conf 'timer) ;; every X seconds
	  (org-preview-html--run-with-timer))
	 ((eq conf 'export) ;; On export using org-html-export-html command manually
	  (advice-add 'org-html-export-to-html :after #'org-preview-html--reload-preview))
	 ((eq conf 'instant) ;; WIP Instantly (on self insert refresh)
	  (add-hook 'post-self-insert-hook #'org-preview-html-refresh nil t)))))

(defun org-preview-html--unconfig ()
  "Unconfigure 'org-preview-html-mode' (remove hooks and advice)."
  (let ((conf org-preview-html-refresh-configuration))
	(cond ((eq conf 'instant) ;; WIP
		   (remove-hook 'post-self-insert-hook #'org-preview-html-refresh t))
		  ((eq conf 'save)
		   (remove-hook 'after-save-hook #'org-preview-html-refresh t))
		  ((eq conf 'timer)
		   (cancel-timer org-preview-html--refresh-timer))
		  ((eq conf 'export)
		   (advice-remove 'org-html-export-to-html #'org-preview-html--reload-preview))))
  (dolist (hook '(kill-buffer-hook kill-emacs-hook)) ;; Remove hooks
    (remove-hook hook #'org-preview-html--stop-preview t))
  ;; Reset variables
  (dolist (var '(org-preview-html--browser-buffer org-preview-html--previewed-buffer-name))
	(set var nil)))

(defun org-preview-html--open-browser ()
  "Open a browser to preview the exported HTML file."
  ;; Store the exported HTML filename
  (setq-local org-preview-html--html-file (concat (file-name-sans-extension buffer-file-name) ".html"))
  (unless (file-exists-p org-preview-html--html-file)
	(org-preview-html--org-export-html)) ;; Unless the file already exists, export it
  ;; Procedure to open the side-by-side preview
  (split-window-right)
  (other-window 1)
  (let ((file org-preview-html--html-file))
	(cond ((eq org-preview-html-viewer 'xwidget) (xwidget-webkit-browse-url (concat "file://" file)))
		  ((eq org-preview-html-viewer 'eww) (eww-open-file file))))
  (setq org-preview-html--browser-buffer (get-buffer (buffer-name)))
  (org-preview-html--previous-window-any-frame))

(defun org-preview-html--start-preview ()
  "Begin the org-preview-html preview."
  (when buffer-file-name
	(cond ((derived-mode-p 'org-mode)
		   (message "org-preview-html has recieved a major update - xwidgets support, refresh configurations and more! \n M-x customize-group org-preview-html-mode")
		   (org-preview-html--open-browser)
		   (org-preview-html--config))
		  (t
		   (org-preview-html-mode -1)
		   (user-error "`%s' not supported by org-preview-html preview, only `org mode'!" major-mode)))))

(defun org-preview-html--stop-preview ()
  "Stop the org-preview-html preview."
  (org-preview-html--kill-preview-buffer)
  (org-preview-html--unconfig))


;;;###autoload
(define-minor-mode org-preview-html-mode
  "(Optionally) live preview for Org exports to HTML."
  :lighter " org-preview-html"
  (if org-preview-html-mode
      (org-preview-html--start-preview)
    (org-preview-html--stop-preview)))

(provide 'org-preview-html)

;;; org-preview-html.el ends here
