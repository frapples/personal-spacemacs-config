(defun user-config/org ()
  " 和org-mode相关的设置"

  (setq org-directory "~/writing/org-notes") ; 默认的org目录

  ;; 各种org相关组件的配置，快捷键设置

  ;; (setq org-ehtml-docroot (expand-file-name "~/writing/notes"))
  ;; org-agenda的文件设置
  (setq org-agenda-files (list org-directory))
  (setq org-default-notes-file (expand-file-name "inbox.org" org-directory))


  ;; calendar日历设置，设置个人的纪念日，个人的纪念日写在单独的文件里
  (load-file (expand-file-name "person-private.el" org-directory))
  (setq my-holidays (append my-holidays (user-private/person-holiday)))
  ;; 个人纪念日设为重要，会用另外的颜色标记
  (setq cal-china-x-important-holidays (user-private/person-holiday))



  ;; org对plantuml和ditaa的支持，对应的程序路径
  (setq org-plantuml-jar-path (expand-file-name "~/.spacemacs.d/others/plantuml.8046.jar"))
  (setq org-ditaa-jar-path "~/.spacemacs.d/others/ditaa.jar")


  ;; deft的笔记路径
  (setq deft-directory "~/writing/notes")

  ;; org-pomodoro计时显示在mode line上
  (spacemacs/toggle-mode-line-org-clock-on)

  (with-eval-after-load 'org
    (user-config/org-agenda)
    (user-config/org-capture)
    (user-config/org-babel)
    (user-config/org-export)
    (user-config/org-others)
    (user-config/org-mobileorg)))


(defun user-config/org-export ()
  (require 'ox-freemind)
  (setq org-export-backends
        '(icalendar ascii html md latex odt org freemind))

  (with-eval-after-load 'ox
    (setq org-export-with-author nil)

    (setq org-html-with-latex 'verbatim)
    (setq org-export-with-latex 'verbatim))


  ;; 一个简单的主题管理
  (setq user-config/org-html-themes-dir "~/writing/css/")
  (setq user-config/org-html-theme "solarized-light.css")

  ;; 用这个函数很方便的修改主题
  (defun user-function/org-set-html-theme ()
    (interactive)
    (let (theme)
      (setq theme (ivy-read "Themes: " (directory-files user-config/org-html-themes-dir nil ".*\\.css$")))
      (setq user-config/org-html-theme theme)
      (message (format  "org-mode导出的html主题已设置为%s" theme))))

  (spacemacs/set-leader-keys-for-major-mode 'org-mode (kbd "e T") 'user-function/org-set-html-theme)

  (defun user-config/org-html-theme-path ()
    (let (path)
      (setq path (expand-file-name user-config/org-html-theme user-config/org-html-themes-dir))
      (unless (file-exists-p path)
        (message (format "You css file %s not exists." path)))
      path))


  ;; 内联css https://stackoverflow.com/questions/19614104/how-to-tell-org-mode-to-embed-my-css-file-on-html-export
  (defun my-org-inline-css-hook (exporter)
    "Insert custom inline css"
    (when (eq exporter 'html)
      (let* ((dir (ignore-errors (file-name-directory (buffer-file-name))))
             (path (concat dir "style.css"))
             (homestyle (or (null dir) (null (file-exists-p path))))
             (final (if homestyle (user-config/org-html-theme-path) path))) ;; <- set your own style file path
        (setq org-html-head-include-default-style nil)
        (setq org-html-head (concat
                             "<style type=\"text/css\">\n"
                             "<!--/*--><![CDATA[/*><!--*/\n"
                             (with-temp-buffer
                               (insert-file-contents final)
                               (buffer-string))
                             "/*]]>*/-->\n"
                             "</style>\n")))))

  (add-hook 'org-export-before-processing-hook 'my-org-inline-css-hook)
  )


(defun user-config/org-agenda ()

  ;; 归档路径配置，这个被我的layer扩展过
  (setq org-archive-location "{org-path-noext}-archive/{year}/{year}-{month}.org")

  (defun user-function/org-query-gtd-active-project ()
    "自己是TODO项并且有TODO项的子节点"
    (and
     (not (org-query-parent (org-query-stringmatch "^Someday / Maybe")))
     (org-query-child (org-query-todo))
     (org-query-todo)))


  (with-eval-after-load 'org-agenda
  ;; 更多参考  https://github.com/remyhonig/org-query
    (setq org-agenda-custom-commands
          '(
            ("n" "TODO清单"
             ((tags-todo "/+NEXT|+TODO"
                         ((org-agenda-overriding-header "TODO清单：")
                          (org-tags-match-list-sublevels nil)
                          (org-agenda-skip-function
                           (org-query-select "tree" (org-query-todo)))
                          (org-agenda-todo-ignore-scheduled t)
                          (org-agenda-todo-ignore-deadlines t)
                          (org-agenda-sorting-strategy
                           '(priority-down category-keep))
                          ))

              (tags-todo "/+NEXT|+TODO|+WAITING|+DEFERRED"
                         ((org-agenda-overriding-header "Waiting清单：")
                          (org-tags-match-list-sublevels nil)
                          (org-agenda-skip-function
                           (org-query-select "tree"
                                             (or (org-query-todo '("WAITING" "DEFERRED"))
                                                 (and (org-query-todo)
                                                      (org-query-child (org-query-todo '("WAITING" "DEFERRED")))))))
                          (org-agenda-todo-ignore-scheduled t)
                          (org-agenda-todo-ignore-deadlines t)
                          (org-agenda-sorting-strategy
                           '(priority-down category-keep))
                          ))))


            ("k" "Maybe清单"
             ((tags "+MAYBE"
                    ((org-agenda-overriding-header "Maybe清单：")
                     (org-tags-match-list-sublevels 'nil)
                     (org-agenda-sorting-strategy
                      '(category-keep))))
              ))
            ))

    (setq org-agenda-custom-commands (append org-agenda-custom-commands (user-private/agenda-custom-commands)))))


(defun user-config/org-capture ()
  (with-eval-after-load 'org-capture
    (setq org-capture-templates (user-private/capture-templates))))


(defun user-config/org-mobileorg ()
  (setq org-mobile-files (list (expand-file-name "gtd-demo.org" org-directory))) ; 被同步的文件

  ;; 当pull时，mobileorg.org的内容会被附加到这个文件来（注：这个文件不需要被同步）：
  (setq org-mobile-inbox-for-pull (expand-file-name "mobile-computer-inbox.org" org-directory))

  ;; 同步盘
  (setq org-mobile-directory "~/org-sync")

  ;; 加密
  (setq org-mobile-use-encryption t)
  (load-file (expand-file-name "mobile-pwd.el" org-directory)))


(defun user-config/org-babel ()
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((sh . t)
     (python . t)
     ;; (R . t)
     ;; (ruby . t)
     ;; (sqlite . t)
     ;; (perl . t)
     (C . t)
     (java . t)
     (plantuml . t)
     (dot . t)
     (latex . t)))


  (setq org-babel-default-header-args
        (cons '(:results . "output replace")
              (assq-delete-all :results org-babel-default-header-args)))


  ;; 不过对于我的那个博客博文，这个没用要手动加，因为它在转换的时候不会加载本配置文件
  (setq org-babel-default-header-args
        (cons '(:exports . "both")
              (assq-delete-all :exports org-babel-default-header-args)))

  (setq org-latex-create-formula-image-program 'dvipng)

  (spacemacs/set-leader-keys-for-minor-mode 'org-src-mode
    "q" 'org-edit-src-exit
    "Q" 'org-edit-src-abort))

(defun user-config/org-others ()
  (setq org-enforce-todo-dependencies t)

  (defun user-function/org-summary-todo (n-done n-not-done)
    "Switch entry to DONE when all subentries are done, to TODO otherwise."
    (let (org-log-done org-log-states)   ; turn off logging
      (org-todo (if (= n-not-done 0) "DONE" "TODO"))))
  (add-hook 'org-after-todo-statistics-hook 'user-function/org-summary-todo))

