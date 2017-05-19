

(defun user-function/unique-file-path (prefix)
  (let ((filename
         (concat (file-name-base (buffer-file-name)) "/"
                 prefix "_"
                 (format-time-string "%Y-%m-%d_%H-%M-%S")
                 ".png")))

    (unless (file-exists-p (file-name-directory filename))
      (make-directory (file-name-directory filename)))
    filename))




;; 使用eyebrowse实现vim的tab概念
(defun user-function/eyebrowse-update-frame-title ()
  "更新GUI的标题栏，将其显示为eyebrowse的页签名称以模拟tab栏"

  ;; 对window-state对象，spacemacs有一系列函数用来对其操作

  (let ((str ""))
    (dolist (config (eyebrowse--get 'window-configs))
      (let (slot win-state tag current-slot name)
        (setq slot (car config))
        (setq tag (nth 2 config))
        (setq current-slot (eyebrowse--get 'current-slot))

        ;; 如果是当前slot，可能记录的数据是旧的
        (if (eq slot current-slot)
            (setq win-state (cadr (eyebrowse--current-window-config current-slot "")))
          (setq win-state (cadr config)))

        (setq name (user-function/window-state-get-selected-buffer-name win-state))
        (if (string= name "")
            (setq name tag))

        (eyebrowse-rename-window-config slot name)

        (if (eq slot current-slot)
            (setq str (concat str "│ " (number-to-string slot) "." name " │"))
          (setq str (concat str "  " (number-to-string slot) "." name "  ")))))

    (setq frame-title-format str)))


(defun user-function/window-state-get-selected-buffer-name (state)
  (let ((buffer-infos
         (delq nil (spacemacs/window-state-walk-windows state
                                                        '(lambda (win)
                                                           (spacemacs/window-state-get-buffer win)))))
        (selected-name ""))

    (dolist (buffer-info buffer-infos)
      (if (cdr (assq 'selected buffer-info))
        (setq selected-name (car buffer-info))))
    selected-name))


;; (defun user-function/window-state-get-last-buffer-name (state)
;;   (let ((leafs (delq nil (spacemacs/window-state-walk-windows (window-state-get nil t)
;;                                                         '(lambda (win)
;;                                                            (if (eq (car win) 'leaf) win nil)))))
;;         (selected-name ""))
;;     (dolist (leaf leafs)
;;       (if (assq 'last leaf)
;;           (setq selected-name (car (cdr (assq 'buffer leaf))))))
;;     selected-name))



(defun user-function/eyebrowse-max-slot-number ()
  (apply 'max (mapcar 'car (eyebrowse--get 'window-configs))))

(defun user-function/eyebrowse-slot-count ()
  (length (mapcar 'car (eyebrowse--get 'window-configs))))

(defun user-function/eyebrowse-reindex-slot ()
  (let (old-to-new-table i)
    (setq old-to-new-table '())

    (setq i 1)
    (eyebrowse--set 'window-configs
      (mapcar
       '(lambda (config)
          (let (new-config)
            (setq old-to-new-table (plist-put old-to-new-table (car config) i))
            (setq new-config (cons i (cdr config)))
            (setq i (+ i 1))
            new-config))
       (eyebrowse--get 'window-configs)))

    (eyebrowse--set 'current-slot (plist-get old-to-new-table (eyebrowse--get 'current-slot)))
    (let ((new-last-slot (plist-get old-to-new-table (eyebrowse--get 'last-slot))))
      (if (eq new-last-slot nil)
          (eyebrowse--set 'last-slot (eyebrowse--get 'current-slot))
        (eyebrowse--set 'last-slot new-last-slot)))))
