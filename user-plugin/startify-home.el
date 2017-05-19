;; 依赖spacemacs-buffer，并且抄袭spacemacs-buffer


(defconst startify-home-name "*Startify-home*"
  "The name of the spacemacs buffer.")


(defvar startify-home-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map [down-mouse-1] 'widget-button-click)
    (define-key map (kbd "RET") 'widget-button-press)

    (define-key map [tab] 'widget-forward)
    (define-key map (kbd "j") 'widget-forward)
    (define-key map (kbd "C-i") 'widget-forward)

    (define-key map [backtab] 'widget-backward)
    (define-key map (kbd "k") 'widget-backward)

    (define-key map (kbd "C-r") 'spacemacs-buffer/refresh)

    (define-key map (kbd "h") '(lambda () (interactive)))
    (define-key map (kbd "l") '(lambda () (interactive)))
    map)
  "Keymap.")

(defvar startify-home-custom-header "******** STARTIFY-HOME ********")
(defvar startify-home-custom-footer "--------- 舒适的emacs --------")

(defvar startify-home-custom-config '(
                                      (todo . 10)
                                      (file . 10)
                                      (project . 10)
                                      )
  "配置列表组的顺序，和最大显示的条目数")

(with-eval-after-load 'evil
  (evil-make-overriding-map startify-home-mode-map 'motion))

(define-derived-mode startify-home-mode fundamental-mode "startify-home"
  "Major mode for startup screen.

\\<startify-home-mode-map>
"
  :group 'spacemacs
  :syntax-table nil
  :abbrev-table nil
  (page-break-lines-mode)
  (setq buffer-read-only t
        truncate-lines t)
  ;; needed to make tab work correctly in terminal
  (evil-define-key 'motion startify-home-mode-map
    (kbd "C-i") 'widget-forward)
  ;; motion state since this is a special mode
  (evil-set-initial-state 'startify-home-mode 'motion))

(defun startify-home/home ()
  (interactive)
  (with-current-buffer (get-buffer-create startify-home-name)
    (save-excursion

      ;; 清空buffer
      (when (> (buffer-size) 0)
        (let ((inhibit-read-only t))
          (erase-buffer)))

      ;; 给buffer插入内容 =_= 对elisp大量依赖动态作用域表示无语
      (let ((buffer-read-only nil))
        (startify-home//insert))

      ;; 设置该buffer的主模式
      (startify-home-mode)))

  ;; 切换到此buffer
  (switch-to-buffer startify-home-name))

(defvar startify-home-link-count 0)

(defun startify-home//insert ()
  (let ((s startify-home-custom-header))
    (add-face-text-property 0 (length s) 'font-lock-function-name-face nil s)
    (add-face-text-property 0 (length s) '(:weight bold) nil s)
    (insert s))

  ;; (insert (propertize startify-home-custom-header 'face 'font-lock-function-name-face))
  (insert "\n")

  (setq startify-home-link-count 0)

  (let ((make-func (list
                    (cons 'file 'startify-home//file-group/make)
                    (cons 'project 'startify-home//project-group/make)
                    (cons 'todo 'startify-home//todo-group/make))))

      (mapc (lambda (i)
              (let ((type (car i))
                    (max-num (cdr i)))
                (startify-home//group/insert
                 (funcall (assoc-default type make-func) max-num))))
            startify-home-custom-config))


  (startify-home//group/insert
   (startify-home//group/make
    (startify-home//group-title/make "")
    (startify-home//group-list/make
     '("q")
     (lambda (item) "quit")
     (lambda (item) item)
     'do)

    (lambda (name)
      (quit-window))))



  (let ((s startify-home-custom-footer))
    (add-face-text-property 0 (length s) 'font-lock-function-name-face nil s)
    (add-face-text-property 0 (length s) '(:weight bold) nil s)
    (insert s))
  )

(defun startify-home//file-group/make (max-num)
  (startify-home//group/make
   (startify-home//group-title/make "最近文件" "f")

   (startify-home//group-list/make
    (spacemacs//subseq recentf-list 0 max-num)
    (lambda (file)
      (abbreviate-file-name file)))

   (lambda (file)
     (find-file-existing file))))

(defun startify-home//project-group/make (max-num)
  (startify-home//group/make

   (startify-home//group-title/make "最近项目" "p")

   (startify-home//group-list/make
    (spacemacs//subseq (projectile-relevant-known-projects) 0 max-num))

   (lambda (file)
     (find-file-existing file))))

(defun startify-home//todo-group/make (max-num)
  (startify-home//group/make

   (startify-home//group-title/make "TODOS" "t")

   (startify-home//group-list/make
    (sort
     (spacemacs//subseq (spacemacs-buffer//todo-list) 0 max-num)
     (lambda (a b)
       (cond
        ((eq "" (cdr (assoc "time" b)))
         t)
        ((eq "" (cdr (assoc "time" a)))
         nil)
        (t
         (string< (cdr (assoc "time" a))
                  (cdr (assoc "time" b)))))))
    (lambda (item)
      (format "%s %s %s"
              (abbreviate-file-name (assoc-default "file" item))
              (if (not (eq "" (assoc-default "time" item)))
                  (format "- %s -" (assoc-default "time" item))
                "-")
              (assoc-default "text" item))))

   (lambda (file)
     (spacemacs-buffer//org-jump file))))

(defun startify-home//group/make (group-title group-list action-func)
  (list
   (cons 'title group-title)
   (cons 'list group-list)
   (cons 'action action-func)))

(defun startify-home//group-title/make (display-name &optional shortcut)
  (list
   (cons 'name display-name)
   (cons 'shortcut shortcut))
  )

(defun startify-home//group-list/make (list &optional to-string-func shortcut-func shortcut-type)
  (setq shortcut-type
        (if shortcut-type
            shortcut-type
          'jump))

  (setq to-string-func
        (if to-string-func
            to-string-func
          (lambda (_) _)))

  (unless shortcut-func
    (setq shortcut-func
          (lambda (_)
            (setq startify-home-link-count (+ startify-home-link-count 1))
            (format "%02d" startify-home-link-count))))

  (list
   (cons 'list list)
   (cons 'to-string to-string-func)
   (cons 'get-shortcut shortcut-func)
   (cons 'shortcut-type shortcut-type)))

(defun startify-home//group/insert (group)
  (let ((group-title (assoc-default 'title group))
        (group-list (assoc-default 'list group))
        (action-func (assoc-default 'action group)))
      (insert
       (propertize
        (concat (assoc-default 'name group-title)
                (if (assoc-default 'shortcut group-title)
                    (format "(%s):"  (assoc-default 'shortcut group-title))
                  ""))
        'face 'font-lock-keyword-face))
    (insert "\n")

    (when (assoc-default 'shortcut group-title)
      (define-key startify-home-mode-map
        (kbd (assoc-default 'shortcut group-title))
        (lexical-let ((search (assoc-default 'name group-title)))
          (lambda ()
            (interactive)
            (startify-home//buffer-search search)))))

    (mapc (lambda (file)

            (let ((key (funcall (assoc-default 'get-shortcut group-list) file)))

              (define-key startify-home-mode-map
                (kbd key)
                (lexical-let ((key key)
                              (file file)
                              (action-func action-func))

                  (if (eq (assoc-default 'shortcut-type group-list) 'jump)
                      (lambda ()
                        (interactive)
                        (startify-home//buffer-search (format "[%s]" key)))
                    (lambda ()
                      (interactive)
                      (funcall action-func file))
                    )))

              (insert "    ")

              (insert (propertize "[" 'face 'font-lock-function-name-face))

              ;; www.gnu.org/software/emacs/manual/html_mono/widget.html#User-Interface
              (widget-create 'push-button
                             ;; 这种方法闭的是值，而且还是愚蠢的替换
                             ;; :action `(lambda (&rest ignore)
                             ;;            (funcall ,action-func ,file))

                             :action (lexical-let ((action-func action-func)
                                                   (file file))
                                       (lambda (&rest ignore)
                                         (funcall action-func file)))

                             :mouse-face 'highlight
                             :follow-link "\C-m"
                             :button-prefix ""
                             :button-suffix ""

                             :button-face 'org-link

                             :format "%[%t%]"
                             key)


              (insert (propertize "]" 'face 'font-lock-function-name-face))
              (insert " ")
              (insert (propertize
                       (funcall (assoc-default 'to-string group-list) file) 'face 'font-lock-string-face))
              (insert "\n")))

          (assoc-default 'list group-list))


    (insert "\n"))
  )

(defun startify-home//buffer-search (search)
  (when
      (or (search-forward search (point-max) t)
        (search-backward search (point-min) t))
    (evil-first-non-blank)))

(provide 'startify-home)
