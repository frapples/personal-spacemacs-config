(defun user-config/org ()
  " 和org-mode相关的设置"

  (setq org-directory "~/writing/org-notes") ; 默认的org目录

  ;; 各种org相关组件的配置，快捷键设置

  ;; org-agenda的文件设置
  (setq org-agenda-files (list org-directory))


  ;; org对plantuml和ditaa的支持，对应的程序路径
  (setq org-plantuml-jar-path (expand-file-name "~/.spacemacs.d/others/plantuml.8046.jar"))
  (setq org-ditaa-jar-path "~/.spacemacs.d/others/ditaa.jar")


  (with-eval-after-load 'org
    (user-config/org-babel)
    (user-config/org-export)))


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
