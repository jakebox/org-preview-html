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
