;; org-at-table-p 用来判断是否在表格中，像这类的函数可以实现org中的一键多用
;; 参考自org-open-at-point org-at-table-p
;; http://orgmode.org/worg/dev/org-element-api.html
;; https://www.emacswiki.org/emacs/WhenToUseIf#toc2
(defun user-function/org-file-p ()
  (let (context type)
    (setq context
          (org-element-lineage (org-element-context) '(link) t))
    (setq type (org-element-type context))

    (and (eq type 'link) (string= (org-element-property :type context) "file")))
  )


(defun user-function/org-get-link-path ()
  "光标所在的链接的文件的路径"
  (let (context)
    (setq context
          (org-element-lineage (org-element-context) '(link) t))
    (if (eq 'link (org-element-type context))
        (org-element-property :path context)
      "")))

(defun user-function/org-at-p (types)
  (if (org-element-lineage (org-element-context) types t)
      t
    nil))

(defun user-function/org-link-p ()
  "光标所在的位置，是否是链接"
  (not (string= (user-function/org-get-link-path) "")))

(defun user-function/org-image-p()
  "光标所在的位置，是否是图片"
  (org-file-image-p (user-function/org-get-link-path)))

;; 已经有了相同功能的函数org-at-timestamp-p
(defun user-function/org-timestamp-p ()
  (user-function/org-at-p '(timestamp)))


;; 参考org-insert-heading-after-current
(defun user-function/org-insert-heading (&optional todop pos)
  "todop is t or nil, pos is 'after or 'before"
  (interactive)
  (org-back-to-heading)
  (beginning-of-line)
  (if todop
      (org-insert-todo-heading nil)
    (org-insert-heading))
  (if (not (eq pos 'before))
      (org-move-subtree-down)))

(defun user-function/org-insert-item (&optional checkbox pos)
  (interactive)
  (beginning-of-line)
  (org-insert-item checkbox)
  (if (not (eq pos 'before))
      (org-move-item-down)))


(defun user-function/org-toggle-statistics-cookies ()
  "去掉或加上[/][%]语法"
  (interactive)
  (let ((type (plist-get (user-function/org-find-statistics-cookies) :type)))
    (user-function/org-delete-statistics-cookies)
    (cond ((eq type nil) (user-function/org-insert-statistics-cookies '/))
          ((eq type '/) (user-function/org-insert-statistics-cookies '%))
          ((eq type '%) nil))))

(defun user-function/org-delete-statistics-cookies ()
  (let ((cookie (user-function/org-find-statistics-cookies)))
    (when cookie
      (delete-region (plist-get cookie :begin) (plist-get cookie :end))
      (save-excursion
        (end-of-line)
        (when (eq (char-before) ? )
          (delete-backward-char 1))))))

(defun user-function/org-insert-statistics-cookies (&optional type)
  "type为'/插入[/]语法（默认）， type为'%, 插入[%]语法 "
  (save-excursion
    (end-of-line)
    (insert (concat " " (if (eq type '%) "[%]" "[/]")))
    (org-update-statistics-cookies nil)))

(defun user-function/org-find-statistics-cookies ()
  (save-excursion
    (beginning-of-line)

    (let ((end-point (save-excursion (end-of-line) (point)))

          (search-point (point))
          (cookie nil))
      (while (and (not cookie) search-point)
        (setq search-point (re-search-forward "\\[" end-point t))
        (when search-point
          (forward-char -1)
          (setq cookie (cadr (org-element-statistics-cookie-parser)))
          (forward-char 1)))
      (if cookie
          (plist-put cookie :type (if (eq (string-match-p "%" (plist-get cookie :value)) nil) '/ '%))
        cookie))))

(defun user-function/notification (title message)
  "Send a notification"
  (when (spacemacs/system-is-linux)
    (require 'notifications)
    (notifications-notify
       :title title
       :body message
       :app-icon (concat spacemacs-assets-directory "spacemacs.svg")
       :urgency 'low)))



;; 参考至： https://emacs-china.org/t/org-mode/79
(defun user-function/org-screenshot ()
  "Take a screenshot into a time stamped unique-named file in the
same directory as the org-buffer and insert a link to this file."
  (interactive)
  (org-display-inline-images)

  (setq filename
        (concat (user-function/ensure-dir (user-function/org-note-asset-dir (buffer-file-name)))
                "screenshot-" (user-function/make-file-name-by-time) ".png"))

  ;; 截图
  (cond ((eq system-type 'darwin)
         (call-process-shell-command "screencapture" nil nil nil nil " -s " (concat "\"" filename "\"" ))
         (call-process-shell-command "convert" nil nil nil nil (concat "\"" filename "\" -resize  \"50%\"" ) (concat "\"" filename "\"" )))
        ((eq system-type 'gnu/linux)
         (call-process "import" nil nil nil filename)))

  (if (file-exists-p filename)
      (progn
        (insert (concat "[[file:" filename "]]"))
        (org-display-inline-images)

        ;; 确认是否使用
        (when (string= (read-string "确认使用这张截图吗？(y/n)") "n")
          (delete-file filename)
          (undo-tree-undo)))))


(defun user-function/org-note-asset-dir (path &optional relative)
  "我们约定，xxx.org的文件放在同级的xxx文件夹下"
  (let* ((dir (file-name-directory path))
         (file-name (file-name-base path)))
    (if relative
        (concat file-name "/")
      (concat dir file-name "/"))))

(defun user-function/org-note-asset-file-path (file-name)
  (user-function/ensure-dir (user-function/org-note-asset-dir (buffer-file-name)))
  (concat (user-function/org-note-asset-dir (buffer-file-name) t) file-name))

(defun user-function/org-note-asset-rename-content-update (old-dir new-dir)
  "asset目录改变，更新org内容中的对应部分"
  (setq old-dir (file-name-as-directory old-dir))
  (setq new-dir (file-name-as-directory new-dir))
  (save-excursion
    (let ((regexp (format
                   "\\[\\[%s%s\\([^:]*\\)\\]\\]"
                   (regexp-opt '("file:" ""))
                   (regexp-quote old-dir))))
      (goto-char (point-min))
      (while (re-search-forward regexp nil t)
        (replace-match
         (save-match-data
           (format "[[file:%s%s]]" new-dir (match-string 1))))))))
