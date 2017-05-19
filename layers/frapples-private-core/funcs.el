;; 这里面放的是我自己定义的一些辅助性函数

(defun user-function/do-and-keep-window(func &optional func-data)
  "执行func，并且执行完毕后光标仍然留在原来的窗口"
  (let* ((origin-window (selected-window))
         (result (apply func func-data)))
    (select-window origin-window)
    result))

(defun user-function/recompile-plugin ()
  (interactive)
  (dolist (path (user-function/directory-files-recursive "~/.emacs.d/elpa/" ".*\\.el$"))
    (let ((elc (concat (file-name-directory path) (file-name-base path) ".elc")))
      (if (f-exists-p elc)
          (byte-compile-file path)))))


(defun user-function/directory-files-recursive (directory match)
  (let ((result '()))
    (dolist (path (directory-files directory t nil t))
      (let ((basename (file-name-nondirectory path)))
        (if (not (member basename '("." "..")))
            (if (f-directory-p path)
                (setq result (append result (user-function/directory-files-recursive path match)))
              (if (equal (string-match match basename) 0)
                  (setq result (cons path result)))))))
    result))


(defun user-function/filter (func seq)
  (let ((new-seq nil))
    (dolist (item seq)
      (when (funcall func item)
        (push item new-seq)))
    (nreverse new-seq)))



;; current-time time-less-p
;; elisp 可以用catch throw模拟break
(defun user-function/current-time-in-which (time-list)
  (let* ((now (format-time-string "%H:%M"))
         (less-time-list
          (user-function/filter
           '(lambda (time)
              (not (user-function/time-less-p now time)))
           time-list)))
    (car (last (if less-time-list less-time-list time-list)))))




(defun user-function/time-less-p (time-a time-b)
  (< (user-function/time-string-to-seconds time-a)
     (user-function/time-string-to-seconds time-b)))

(defun user-function/time-string-to-seconds (time-string)
  (let ((lst (mapcar 'string-to-number (split-string time-string ":"))))
    (+ (* 60 60 (car lst)) (* 60 (cadr lst)))))


(defun user-function/get-string-from-file (file-path)
  "Return filePath's file content."
  (with-temp-buffer
    (insert-file-contents file-path)
    (buffer-string)))

(defun user-function/ensure-dir (file-path)
  (unless (file-exists-p file-path)
    (mkdir file-path))
  file-path)

(defun user-function/ensure-orgfile-image-dir ()
  (user-function/ensure-dir (file-name-base (buffer-file-name))))
