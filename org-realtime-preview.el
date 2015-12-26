(require 'org)
(require 'eww)

(defvar org-realtime-preview-output-file-name "realtime-preview-result.html"
  "预览产生的临时文件名称")

(defun org-realtime-preview--select-or-create-buffer-window (buffer-or-name)
  "若frame中有显示`buffer-or-name'的window,则选中该window,否则创建新window显示该buffer"
  (let ((buf (get-buffer-create buffer-or-name)))
	(unless (get-buffer-window buf)
	  (split-window)
	  (switch-to-buffer buf))
	(select-window (get-buffer-window buf))))

(defun org-realtime-preview--buffer-point(buffer-or-name &optional default-point)
  "获取指定buffer的光标所在位置

若参数`buffer-or-name'没有对应buffer,则返回`default-point'"
  (if (get-buffer buffer-or-name)
	  (with-current-buffer buffer-or-name
		(point))
	default-point))
(defun org-realtime-preview-convert (output-file-name)
  "导出org为`output-file-name'中,并调用`eww-open-file'来查看"
  (let ((cb (current-buffer))
		(eww-point (org-realtime-preview--buffer-point "*eww*" 1)))
    (save-excursion
	  (org-realtime-preview--select-or-create-buffer-window "*eww*")
	  (with-current-buffer cb
		(org-export-to-file 'html output-file-name nil nil nil nil nil #'eww-open-file))
	  (goto-char eww-point))
    (org-realtime-preview--select-or-create-buffer-window cb)))

(defun org-realtime-preview ()
  "导出org为`org-realtime-preview-output-file-name'中,并调用`eww-open-file'来查看"
  (interactive)
  (org-realtime-preview-convert org-realtime-preview-output-file-name))

(defun turn-on-org-realtime-preview ()
  "开启保存后自动预览"
  (interactive)
  (add-hook 'after-save-hook #'org-realtime-preview nil t))

(defun turn-off-org-realtime-preview ()
  "关闭保存后自动预览"
  (interactive)
  (remove-hook 'after-save-hook #'org-realtime-preview t))

(provide 'org-realtime-preview)
