(with-eval-after-load 'org-agenda
  (setq org-agenda-entry-types (remove ':timestamp org-agenda-entry-types))

  ;; 设置agenda对每天的显示字符
  (setq org-agenda-format-date
        (lambda (date)
          (let ((lunar-date (user-function/date-to-lunar-date date)))
            (format "%04d-%02d-%02d %s(%s%s%s)%s"
                    (calendar-extract-year date)
                    (calendar-extract-month date)
                    (calendar-extract-day date)
                    (substring (cal-china-x-day-name date) 2)
                    (if (user-function/lunar-leap-month-p lunar-date) "闰" "")
                    (user-function/lunar-month-name lunar-date)
                    (user-function/lunar-day-name lunar-date)

                    (let ((holidays (user-function/get-holidays date)))
                      (if holidays
                          (concat  " " (string-join holidays " "))
                        ""))
                    )))))


(add-hook 'org-capture-mode-hook
          (lambda ()
            (setq-local org-complete-tags-always-offer-all-agenda-tags t)))

(with-eval-after-load 'org-mobile
  (advice-add
   'org-mobile-move-capture
   :after
   (lambda ()
     "TODO 还未实现，思路是临时修改org-mobile-inbox-for-pull将其暂时移动到另外一个文件，之后这个函数将所有的caputre移动到指定的子标题下"
     ))
  )
