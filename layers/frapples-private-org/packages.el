(defconst frapples-private-org-packages
  '(
    ;; Get the package from MELPA, ELPA, etc.
    org-ehtml
    (org-ehtml :location elpa)

    org-download
    org-octopress

    (org-wiki :location (recipe :fetcher github :repo "caiorss/org-wiki"))
    ))

(defun frapples-private-org/init-org-wiki ()
  ;; 抄袭自： https://github.com/syl20bnr/spacemacs/pull/8447/files
  (defun spacemacs/orgwiki-insert-asset-link ()
    "Insert link [[file:<page>/<file>]] to asset file of current page at point.
+Insert an asset file of current page at point providing a Helm completion.
+Example: [[Linux/LinuxManual.pdf]]"
    (interactive)
    (let ((pagename (file-name-base (buffer-file-name))))
      (org-wiki--asset-helm-selection
       pagename
       (lambda (file)
         (insert (format "[[file:%s/%s]]"
                         pagename
                         file
                         )))))
    (when org-startup-with-inline-images
      (org-display-inline-images)))

  (use-package org-wiki)

  (setq org-wiki-location "~/writing/wiki")
  )

(defun frapples-private-org/init-org-ehtml ()

  ;; https://github.com/eschulte/org-ehtml
  (defvar user-conifg/org-ehtml-server
    nil
    "")


  (setq org-ehtml-everything-editable t)
  (setq org-ehtml-allow-agenda t))


(defun user-function/org-ehtml-server-start ()
  (interactive)
  (require 'org-ehtml)
  (unless user-conifg/org-ehtml-server
    (setq user-conifg/org-ehtml-server
          (ws-start org-ehtml-handler 8888))))

(defun user-function/org-ehtml-server-stop ()
  (interactive)
  (require 'org-ehtml)
  (when user-config/org-ehtml-server
    (ws-stop user-config/org-ehtml-server)))




(defun frapples-private-org/post-init-deft ()
  (setq deft-extensions '("org" "md" "txt"))
  (spacemacs/set-leader-keys-for-major-mode 'deft-mode "q" 'quit-window)
  (evil-define-key 'normal deft-mode-map "q" 'quit-window)
  (setq deft-recursive t)

  (setq deft-file-naming-rules '((noslash . "-")
                                 (nospace . ""))))


(defun frapples-private-org/post-init-org-download ()

  ;; https://emacs-china.org/t/org-download/2422/3
  (defun custom-org-download-method (link)
    (org-download--fullname (org-link-unescape link)))
  (setq org-download-method 'custom-org-download-method) ; 注意：这里不能用lambda表达式

  ;; 顺便改下annotate，就是自动插入的那行注释，里面写的是图片来源路径
  (setq org-download-annotate-function
        '(lambda (link)
           (org-download-annotate-default (org-link-unescape link))))


  ;; https://emacs-china.org/t/org-download/1798/7
  ;; https://github.com/abo-abo/org-download
  ;; 设置保存路径
  (add-hook 'org-mode-hook '(lambda ()
                              (setq org-download-image-dir (file-name-base (buffer-name)))
                              (setq org-download-heading-lvl nil))))


;; 抄的配置：http://www.tuicool.com/articles/reaUVzF
(defun frapples-private-org/init-org-octopress ()
  (use-package org-octopress
    :config
    (progn
      (setq org-octopress-directory-top       "~/hexo-blog/source")
      (setq org-octopress-directory-posts     "~/hexo-blog/source/_posts")
      (setq org-octopress-directory-org-top   "~/hexo-blog/source")
      (setq org-octopress-directory-org-posts "~/hexo-blog/source/_posts")
      )

    ;; rewrite in org-octopress.el
    (defun org-octopress--summary-table (contents keymap) ;; 去掉 publish 这一列，因为 hexo 不需要
      (let ((param (copy-ctbl:param ctbl:default-rendering-param)))
        (ctbl:create-table-component-region
         :param param
         :width  nil
         :height nil
         :keymap keymap
         :model
         (make-ctbl:model
          :data contents
          :sort-state '(-1 2)
          :column-model
          (list (make-ctbl:cmodel
                 :title "Date"
                 :sorter 'ctbl:sort-string-lessp
                 :min-width 10
                 :align 'left)
                (make-ctbl:cmodel
                 :title "Category"
                 :align 'left
                 :sorter 'ctbl:sort-string-lessp)
                (make-ctbl:cmodel
                 :title "Title"
                 :align 'left
                 :min-width 40
                 :max-width 140)
                )))))

    (defun org-octopress (&optional title)
      "Org-mode and Octopress."
      (interactive)
      (setq org-octopress-summary-buffer (get-buffer-create "Octopress"))
      (switch-to-buffer org-octopress-summary-buffer)
      (setq buffer-read-only nil)
      (erase-buffer)
      (insert (org-octopress--summary-header title))
      (save-excursion
        (setq org-octopress-component (org-octopress--summary-table
                                       (org-octopress--scan-post) org-octopress-summary-mode-map)))
      (ctbl:cp-add-click-hook
       org-octopress-component
       (lambda ()
         (find-file (nth 3 (ctbl:cp-get-selected-data-row org-octopress-component))))) ;; 这里的 4 改为 3，因为我修改了列数
      (org-octopress-summary-mode)
      (ctbl:navi-goto-cell
       (ctbl:find-first-cell (ctbl:component-dest org-octopress-component)))
      )

    (define-key org-octopress-summary-mode-map "w" 'user-function/new-open-post)
    (defun user-function/new-open-post ()
      (interactive)
      ;TODO
      )

    (defun org-octopress--scan-post ()
      (mapcar
       (lambda (filename)
         (org-jekyll-property
          '(:date
            :jekyll-categories
            :title
            :input-file)
          filename))
       (directory-files
        (expand-file-name
         org-octopress-directory-org-posts) t "^.*\\.org$"))) ;; jekyll 要求所有文章以日期开头，而 hexo 不需要


    ;; rewrite in ox-jekyll.el
    (defcustom org-jekyll-date ""
      "Default date used in Jekyll article."
      :group 'org-export-jekyll
      :type 'string)
    (org-export-define-derived-backend 'jekyll 'html
                                              ;; :export-block '("HTML" "JEKYLL")
                                              :menu-entry
                                              '(?j "Jekyll: export to HTML with YAML front matter."
                                                   ((?H "As HTML buffer" org-jekyll-export-as-html)
                                                    (?h "As HTML file" org-jekyll-export-to-html)))
                                              :translate-alist
                                              '((template . org-jekyll-template) ;; add YAML front matter.
                                                (src-block . org-jekyll-src-block)
                                                (inner-template . org-jekyll-inner-template)) ;; force body-only
                                              :options-alist
                                              '((:jekyll-layout "LAYOUT" nil org-jekyll-layout) ;; hexo-renderer-org 没有使用 JEKYLL 这个 prefix
                                                (:jekyll-categories "CATEGORIES" nil org-jekyll-categories)
                                                (:jekyll-tags "TAGS" nil org-jekyll-tags)
                                                (:date "DATE" nil org-jekyll-date)
                                                (:jekyll-published "PUBLISHED" nil org-jekyll-published)
                                                (:jekyll-comments "COMMENTS" nil org-jekyll-comments)))
    ))
