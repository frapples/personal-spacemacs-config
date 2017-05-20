
(defun user-function/org-tags-view-only-todo ()
  (interactive)
  (org-tags-view t))


;; user-function/org-tags-view-with-@的之前实现，原理挺奇怪，所以保留
;; http://stackoverflow.com/questions/28039958/emacs-call-a-kbd-macro-in-defun-function
(defun user-function/kbd-macro-query ()
  (interactive)
  (let ((executing-kbd-macro defining-kbd-macro))
    (run-with-timer 0.1 nil 'abort-recursive-edit)
    (recursive-edit)))
(global-set-key (kbd "C-x Q") 'user-function/kbd-macro-query)

(defun user-function/org-tags-view-with-@-1 ()
  (interactive)
  (let ((minibuffer-message-timeout 0))
    (execute-kbd-macro (read-kbd-macro "M-x user-function/org-tags-view-only-todo RET @ C-x Q"))))



(defun user-function/org-tags-view-with-@ ()
  (interactive)
  (org-tags-view t))

(push '(user-function/org-tags-view-with-@ . "^@") ivy-initial-inputs-alist)

(spacemacs/set-leader-keys
  "ao@" 'user-function/org-tags-view-with-@
  "aoT" 'org-todo-list
  "aof" 'user-function/counsel-org-agenda-first-headings
  "aon" (lambda () (interactive) (org-agenda nil "n"))
  "aoP" (lambda () (interactive) (org-agenda nil "p"))
  "aom" (lambda () (interactive) (org-agenda nil "k"))


  ;; 设置calendar快捷键
  ;; 原本的ac被calc占用，给换到aC
  "aoC" 'calendar
  "ac" 'calendar
  "aC" 'calc-dispatch
  )

(with-eval-after-load 'org-agenda
  ;; 取消spacemacs定义的$键（容易误按还没有确认）
  (define-key org-agenda-mode-map (kbd "$") nil)
  ;; 其它我喜欢的按键, 和org-mode中统一
  (define-key org-agenda-mode-map (kbd "-") 'org-agenda-todo)
  (spacemacs/set-leader-keys-for-major-mode 'org-agenda-mode (kbd "-") 'org-agenda-priority-down)

  (define-key org-agenda-mode-map (kbd "RET") 'org-agenda-goto)

  ;; 不能写成 (kbd "TAB") ，因为这无法冲掉已经存在的表项
  (define-key org-agenda-mode-map [(tab)]
    (lambda ()
      (interactive)
      (user-function/do-and-keep-window (lambda () (org-agenda-goto t)))))

  (define-key org-agenda-mode-map (kbd "P") 'org-pomodoro))



(with-eval-after-load 'org
  (evil-define-key 'motion calendar-mode-map
    (kbd "RET") 'user-function/calendar-open-agenda-day
    (kbd "TAB") '(lambda () (interactive)
                   (user-function/do-and-keep-window 'user-function/calendar-open-agenda-day))

    (kbd "t") 'user-function/org-todo-list-without-scheduled
    (kbd "T") 'org-todo-list
    ))

(spacemacs/set-leader-keys-for-minor-mode 'org-capture-mode
  "q" 'org-capture-finalize
  "Q" 'org-capture-kill
  "k" 'org-capture-kill)
