;; 滚轮调整字体大小的设置
(defvar user-config/font--en-size-offset 0)
(defvar user-config/font--zh-size-offset 0)

(defvar user-config/font--config-hook nil)

(defun user-function/set-font-config-hook (v)
  (setq user-config/font--config-hook v)
  (user-function/font-update))

(defun user-function/font-update ()
  (when (display-graphic-p)
    (when (functionp user-config/font--config-hook)
      (let (conf zh-size en-size)
        (setq conf (funcall user-config/font--config-hook))

        (setq zh-size (+ (plist-get conf :zh-size) user-config/font--zh-size-offset))
        (setq en-size (+ (plist-get conf :en-size) user-config/font--en-size-offset))

        (when (<= zh-size 0) (setq zh-size 1))
        (when (<= en-size 0) (setq en-size 1))

        (spacemacs//set-monospaced-font (plist-get conf :en-font) (plist-get conf :zh-font) en-size zh-size)))))

(defun user-function/font-add (n)
  (setq user-config/font--en-size-offset (+ user-config/font--en-size-offset n))
  (setq user-config/font--zh-size-offset (+ user-config/font--zh-size-offset n))
  (user-function/font-update))

(defun user-function/font-sub (n)
  (setq user-config/font--en-size-offset (- user-config/font--en-size-offset n))
  (setq user-config/font--zh-size-offset (- user-config/font--zh-size-offset n))
  (user-function/font-update))

(defun user-function/font-add-1 () (interactive) (user-function/font-add 1))
(defun user-function/font-sub-1 () (interactive) (user-function/font-sub 1))

(defun user-function/font-add-5 () (interactive) (user-function/font-add 5))
(defun user-function/font-sub-5 () (interactive) (user-function/font-sub 5))

(defun user-function/noting ())
(defun user-function/change-font-micro-state ()
  "[+]+1 [-]-1 [j]+5 [k]-5 [q]quit")
(spacemacs|define-micro-state font-size
  :doc (user-function/change-font-micro-state)
  ;; :use-minibuffer t
  ;; :disable-evil-leader t
  :bindings
  ("+" user-function/font-add-1)
  ("-" user-function/font-sub-1)
  ("j" user-function/font-add-5)
  ("k" user-function/font-sub-5)
  ("q" user-function/noting :exit t)
  ("<esc>" user-function/noting :exit t))
