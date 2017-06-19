(defconst frapples-private-org-agenda-packages
  '(
    ;; Get the package from MELPA, ELPA, etc.
    cal-china-x
    (cal-china-x :location elpa)
    org-pomodoro

    (org-archive-treeconserve :location (recipe :fetcher github
                                                :repo "dfeich/org-archive-treeconserve"))

    (org-query :location (recipe :fetcher github
                                 :repo "remyhonig/org-query"))
    ))


(defun frapples-private-org-agenda/init-org-query ()
  "这个是一组和定制org-agenda有关的函数库"

  (use-package org-query)
  (require 'org-query-gtd)
  )

(defun frapples-private-org-agenda/init-org-archive-treeconserve ()
  "这个插件使用后，能够归档时保留原来的树结构。但是org-archive-location中的datetree和标题会失效
  这里配置了按时间归档org代办事项，并保留原来的树结构"
  ;; 看了下代码，这个插件只使用org-archive-location里面::之前的部分
  (use-package org-archive-treeconserve)

  (setq org-archive-file-header-format "")

  (setq org-archive-location "{org-path-noext}-archive/{year}/{year}-{month}.org")
  ;; https://stackoverflow.com/questions/10143959/keeping-the-context-when-archiving-in-emacs-org-mode
  (advice-add
   'org-archive-subtree
   :around
   (lambda (origin-function &rest args)
     (let* ((time (format-time-string
                   (substring (cdr org-time-stamp-formats) 1 -1)))

            (date (org-date-to-gregorian
                   (or (org-entry-get nil "CLOSED" t) time)))

            ;; agenda里这个归档也有效，不是因为这里处理过，而且因为agenda用的另外一个函数包装了
            (filepath-noext (file-name-sans-extension (org-extract-archive-file "%s ::")))
            (org-archive-location-ext org-archive-location)

            (arg-alist (list (cons "year" (format "%d" (caddr date)))
                             (cons "month" (format "%02d" (car date)))
                             (cons "day" (format "%02d" (cadr date)))
                             (cons "org-path-noext" filepath-noext)))

            ;; 利用动态作用域
            (org-archive-location
             (replace-regexp-in-string
              "{[a-zA-Z-]+}"
              (lambda (match)
                (setq match (substring match 1 (- (length match) 1)))
                (assoc-default match arg-alist))
              org-archive-location-ext)))
       (apply origin-function args)
       (org-save-all-org-buffers))))
  )

(defun frapples-private-org-agenda/init-cal-china-x ()
  (use-package cal-china-x)
  (setq mark-holidays-in-calendar t)
  ;; (setq cal-china-x-important-holidays cal-china-x-chinese-holidays)
  ;; (setq calendar-holidays cal-china-x-important-holidays)

  ;; 参考自：http://blog.csdn.net/u014801157/article/details/24372485
  (setq my-holidays
        '(;;公历节日
          (holiday-fixed 1 1 "元旦")
          (holiday-fixed 2 14 "情人节")
          (holiday-fixed 5 1 "劳动节")
          (holiday-float 5 0 2 "母亲节")
          (holiday-fixed 9 10 "教师节")
          (holiday-float 6 0 3 "父亲节")
          (holiday-fixed 10 1 "国庆节")
          ;; 农历节日

          (holiday-lunar 1 1  "春节" 0)
          (holiday-lunar 1 15 "元宵节" 0)
          (holiday-solar-term "清明" "清明节")
          (holiday-lunar 5 5  "端午节" 0)
          (holiday-lunar 7 7  "七夕节" 0)
          (holiday-lunar 8 15 "中秋节" 0)
          (holiday-lunar 9 9  "重阳节" 0)
          ;;纪念日
          ))


  ;; 只显示我自己定义的节日
  (setq calendar-holidays my-holidays)

  ;; 参考cfw:org-open-agenda-day
  (defun user-function/calendar-open-agenda-day ()
    (interactive)
    (let ((date (calendar-cursor-to-nearest-date)))
      (when date
        (org-agenda-list nil (calendar-absolute-from-gregorian date) 'day))))

  ;; 标记今天
  (add-hook 'calendar-today-visible-hook 'calendar-mark-today)

  ;; 通过 SPC ? 可以查看快捷键是那个定义的
  (evil-define-key 'motion calendar-mode-map
    (kbd "h") 'calendar-backward-day
    (kbd "l") 'calendar-forward-day
    (kbd "k") 'calendar-backward-week
    (kbd "j") 'calendar-forward-week
    (kbd "H") 'calendar-backward-month
    (kbd "L") 'calendar-forward-month
    (kbd "K") 'calendar-backward-year
    (kbd "J") 'calendar-forward-year
    (kbd "0") 'calendar-beginning-of-week
    (kbd "$") 'calendar-end-of-week
    (kbd "RET") 'user-function/calendar-open-agenda-day
    (kbd "TAB") '(lambda () (interactive)
                   (user-function/do-and-keep-window 'user-function/calendar-open-agenda-day))

    (kbd "t") 'user-config/org-my-todo-list
    (kbd "T") 'org-todo-list
    )

  ;; 参考emacs官方文档和cal-china-x源代码
  ;; 这个设置可以是一个s表达式，并且这个s表达式可以使用变量year和month =_=
  (setq calendar-month-header '(let* ((date (list month 1 year))
                                      (cn-date (user-function/date-to-lunar-date date)))
                                 (propertize
                                  (format "%d年(%s)%2d月"
                                          year
                                          (user-function/lunar-zodiac-name cn-date)
                                          month)
                                  'font-lock-face 'calendar-month-header))
        )

  (setq calendar-week-start-day 1)
  (setq calendar-day-header-array ["日" "一" "二" "三" "四" "五" "六"])
  (setq calendar-month-name-array
        ["一月(January)" "二月(February)" "三月(March)" "四月(April)" "五月(May)" "六月(June)"
         "七月(July)"    "八月(August)"   "九月(September)" "十月(October)" "十一月(November)" "十二月(December)"])
  (setq calendar-day-name-array
        ["周日" "周一" "周二" "周三" "周四" "周五" "周六"])
  )


(defun frapples-private-org-agenda/post-init-org-pomodoro ()
  (add-hook 'org-pomodoro-finished-hook
            (lambda ()
              (user-function/notification "Pomodoro" "完成！休息一会儿吧！")
              (message "Pomodoro完成，休息一会儿吧！")))
  (add-hook 'org-pomodoro-short-break-finished-hook
            (lambda ()
              (user-function/notification "Pomodoro" "Short Break，继续？")
              (message "Short Break，继续？")))
  (add-hook 'org-pomodoro-long-break-finished-hook
            (lambda ()
              (user-function/notification "Pomodoro" "Long Break，继续？")
              (message "Long Break，继续？")))

  ;; (setq org-pomodoro-audio-player "mplayer") ; 默认是aplay或afplay
  ;; (setq org-pomodoro-finished-sound-args "-volume 0.3")
  ;; (setq org-pomodoro-long-break-sound-args "-volume 0.3")
  ;; (setq org-pomodoro-short-break-sound-args "-volume 0.3")
  )
