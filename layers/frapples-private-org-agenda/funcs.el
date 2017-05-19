;; 参考 https://github.com/emacs-helm/helm
;; 参考helm-org-agenda-files-headings helm-org-heading-class
(defun user-function/helm-org-agenda-first-headings ()
  "列出org-agenda里面的一级标题，点击跳转至对应标题并且启用org-narrow-to-subtree"
  (interactive)
  (helm :sources (helm-build-sync-source "agenda files first headings"
                   :candidates (user-function//helm-counsel-org-agenda-first-headings-candidates)
                   :fuzzy-match t
                   :action (helm-make-actions "agenda files first headings"
                                              'user-function//helm-counsel-org-agenda-first-headings-action))
        :buffer "*helm agenda headings*"))


(defun user-function/counsel-org-agenda-first-headings ()
  "helm-org-agenda-files-headings的ivy版本"

  (interactive)
  (ivy-read "GTD文件标题: "
            (user-function//helm-counsel-org-agenda-first-headings-candidates)
            :action '(lambda (item)
                       (user-function//helm-counsel-org-agenda-first-headings-action
                        (cdr item)))
            :caller 'user-function/counsel-org-agenda-first-headings))

(defun user-function//helm-counsel-org-agenda-first-headings-action (candidate)
  (helm-org-goto-marker candidate)
  (widen)
  (helm-org-goto-marker candidate)
  (org-narrow-to-subtree)
  (org-show-children))

(defun user-function//helm-counsel-org-agenda-first-headings-candidates ()
  (require 'helm-org)
  (let ((candidates
         (helm-org-get-candidates (org-agenda-files))))
    (setq candidates
          (remove-if
           '(lambda (candidate)
              (let ((text (car candidate)))
                ;; (eq (get-text-property 0 'face text) 'org-level-1)
                ;; (get-text-property 0 'helm-real-display text)
                ;; 这里是通过显示的字符串带/来判断的，NOTE: 或许有标题本身就带/的可能
                ;; (string-match-p "/" (substring-no-properties (car candidate)))
                (not (eq (get-text-property (- (length text) 1) 'face text) 'org-level-1))
                ))
           candidates))
    candidates))



;; 下面是和日期和农历相关的函数

;; 农历有关操作参考自cal-china-x源码，而且部分函数依赖于此插件
(defun user-function/date-to-lunar-date (date)
  "把calendar表示的公历日期转换成农历日期"
  (calendar-chinese-from-absolute
  (calendar-absolute-from-gregorian date)))

(defun user-function/lunar-sexagesimal-name (lunar)
  "农历中类似戊戌之类的部分"
  (let ((year (cadr lunar)))
    (calendar-chinese-sexagesimal-name year)))

(defun user-function/lunar-zodiac-name (lunar)
  "农历年对应的生肖"
  (let ((year (cadr lunar)))
    (aref cal-china-x-zodiac-name (% (1- year) 12))))

(defun user-function/lunar-month-name (lunar)
  "月份名"
  (let ((month (cl-caddr lunar)))
      (aref cal-china-x-month-name (1- (floor month)))))

(defun user-function/lunar-leap-month-p (lunar)
  "是否闰月"
  (let ((month (cl-caddr lunar)))
    (not (integerp month))))

(defun user-function/lunar-day-name (lunar)
  "天名（初一初二之类的）"
  (let ((day (cl-cadddr lunar)))
    (aref cal-china-x-day-name (1- day))))

(defun user-function/get-holidays (date)
  (let (all-holidays holidays)

    ;; =_= 这个坑爹的calendar-hoiday-list依赖动态作用域特性
    (setq all-holidays
          (let ((displayed-year (calendar-extract-year date))
                (displayed-month (calendar-extract-month date)))
            (calendar-holiday-list)))

    (setq holidays '())
    (dolist (i all-holidays)
      (when (equal (car i) date)
        (setq holidays (cons (cadr i) holidays))))
    holidays
    ))


