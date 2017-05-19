
(defvar multi-theme--config-table '())

(defvar multi-theme--timer '())

(defun multi-theme-set-timely-themes (table)
  (setq multi-theme--config-table
        (sort
         table
         (lambda (a b)
            (user-function/time-less-p (car a) (car b)))))

  (mapc 'cancel-timer multi-theme--timer)

  ;; run-with-timer run-with-idle-timer run-at-time三个函数可以用来做定时任务
  (dolist (item multi-theme--config-table)
    (pcase item
      (`(,time . ,theme)

       (let (timer)
         (setq timer
               (run-at-time
                time
                (* 60 60 24)
                (lambda (theme)
                  (spacemacs/load-theme theme))
                theme))
         (push timer multi-theme--timer))))))



(defun multi-theme-current ()
  (assoc-default
   (user-function/current-time-in-which (mapcar 'car multi-theme--config-table))
   multi-theme--config-table))

(defun multi-theme-themes (&optional other-themes)
  (delete-dups
   (append
    (list (multi-theme-current))
    (mapcar 'cdr multi-theme--config-table)
    other-themes)))


(provide 'multi-theme)

