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


;; 模仿org-insert-heading-after-current
(defun user-function/org-insert-todo-heading-after-current ()
  (interactive)
  (org-back-to-heading)
  (org-insert-todo-heading nil)
  (org-move-subtree-down)
  (end-of-line 1))

(defun user-function/org-insert-item-after-current (&optional checkbox)
  (interactive)
  (org-insert-item checkbox)
  (org-move-item-down))


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

(defun user-function/notification (message)
  "Send a notification"
  (when (spacemacs/system-is-linux)
    ; shell-command函数也行
    (if (executable-find "gnome-osd-client")
      (call-process "gnome-osd-client" nil nil nil
        "-f" (format "<message id=\"emacs\" osd_fake_translucent_bg=\"off\" animations=\"off\" hide_timeout=\"60000\"> \n
                  <span foreground=\"yellow\">%s</span></message>"
                        message))
      (message "请先安装gnome-osd！"))))



;; 参考至： https://emacs-china.org/t/org-mode/79
(defun user-function/org-screenshot ()
  "Take a screenshot into a time stamped unique-named file in the
same directory as the org-buffer and insert a link to this file."
  (interactive)
  (org-display-inline-images)

  (setq filename (user-function/unique-file-path "screenshot"))


  ;; take screenshot
  (when (eq system-type 'darwin)
    (call-process-shell-command "screencapture" nil nil nil nil " -s " (concat "\"" filename "\"" ))
    (call-process-shell-command "convert" nil nil nil nil (concat "\"" filename "\" -resize  \"50%\"" ) (concat "\"" filename "\"" )))

  (when (eq system-type 'gnu/linux)
    (call-process "import" nil nil nil filename))


  (if (file-exists-p filename)
      (progn
        (insert (concat "[[file:" filename "]]"))
        (org-display-inline-images)

        ;; 确认是否使用
        (when (string= (read-string "确认使用这张截图吗？(y/n)") "n")
          (delete-file filename)
          (undo-tree-undo))))
  )
