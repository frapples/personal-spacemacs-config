(require 'diary-lib)

(defvar multi-theme--config-table (list (cons (diary-entry-time "00:00") nil))
  "一个按照时间顺序排序的链表，每个元素是(cons 时间点 主题)")
(defvar multi-theme-default-theme 'sanityinc-solarized-light)

(defvar multi-theme--timer '())

(defun multi-theme-add-timely-theme (start end theme)
  (setq start (diary-entry-time start))
  (setq end (diary-entry-time end))

  (when (< start end)
    (let (start-find end-find start-new-item end-new-tiem)
      (setq start-find (multi-theme--search-in-sorted-table multi-theme--config-table start 'car))
      (setq end-find (multi-theme--search-in-sorted-table multi-theme--config-table end 'car))

      (assert start-find)
      (assert end-find)

      (setq start-new-item (cons (cons start theme) nil))
      (setq end-new-tiem (cons (cons end (cdr (car end-find))) (cdr end-find)))
      (if (= (car (car start-find)) start)
          (progn
            (setcdr (car start-find) theme)
            (setcdr start-find end-new-tiem))
        (progn
          (setcdr start-find start-new-item)
          (setcdr start-new-item end-new-tiem))))

    (mapc 'cancel-timer multi-theme--timer)
    (setq multi-theme--timer '())

    (dolist (item multi-theme--config-table)
      (pcase item
        (`(,time . ,theme)
         (unless theme
           (setq theme multi-theme-default-theme))
         (let ((timer
                (run-at-time (message "%04d" time) (* 60 60 24) 'spacemacs/load-theme theme)))
           (push timer multi-theme--timer)))))))

(defun multi-theme--search-in-sorted-table (table find &optional key-func)
  "在一个有序的表中，搜索find应该插入的前面元素。如无，返回nil"
  (unless key-func
    (setq key-func (lambda (_) _)))

  (cond ((equal table nil) nil)
        ((< find (funcall key-func (car table))) nil)
        ((and (<= (funcall key-func (car table)) find)
              (or (equal (cadr table) nil)
                  (< find (funcall key-func (cadr table)))))
         table)
        (t (multi-theme--search-in-sorted-table (cdr table) find key-func))))


(defun multi-theme-current ()
  (let ((find
         (multi-theme--search-in-sorted-table
          multi-theme--config-table
          (diary-entry-time (format-time-string "%H:%M"))
          'car)))

    (assert find)

    (cdr (car find))))

(defun multi-theme-themes (&optional other-themes)
  (delete-dups
   (append
    (list (multi-theme-current))
    (mapcar 'cdr multi-theme--config-table)
    other-themes)))


(provide 'multi-theme)

