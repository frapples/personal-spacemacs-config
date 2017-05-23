(defun user-config/base ()
  (user-config/complete)
  (user-config/gui-frame)
  (user-config/chinese)
  (user-config/startify-home)
  )



(defun user-config/complete ()
  ;; auto-complete层的快捷键设置覆盖主模式的快捷键设置
  (global-company-mode)
  ;; https://github.com/tumashu/chinese-pyim#%E7%89%B9%E7%82%B9  chinese-pyim提供的分词功能
  ;; (require 'chinese-pyim-company)
  (setq pyim-company-max-length 6))


(defun user-config/gui-frame ()
  ;; emacsclient连接到server的时候，会执行此钩子函数
  (add-hook
   'after-make-frame-functions
   '(lambda (frame)
      (select-frame frame)
      (when dotspacemacs-maximized-at-startup
        (spacemacs/toggle-maximize-frame-on)))))


(defun user-config/chinese ()
  (ace-pinyin-global-mode)
  ;; 经过测试 似乎没有什么用
  (use-package chinese-word-at-point)

  (user-function/set-font-config-hook
   (lambda ()
     (if (not (spacemacs/system-is-mswindows))
         (if (string= (getenv "USER") "chromebook")
             '(:en-font "DeJaVu sans mono" :zh-font "文泉驿等宽微米黑" :en-size 14 :zh-size 16)
           '(:en-font "Consolas" :zh-font "文泉驿等宽微米黑" :en-size 14 :zh-size 16))
       '(:en-font "Consolas" :zh-font "微软雅黑"  :en-size 14 :zh-size 16))))
  )


(defun user-function/load-my-layout ()
  (interactive)
  (persp-load-state-from-file (concat persp-save-dir "user-layout")))


(defun user-function/save-my-layout ()
  (interactive)
  (persp-save-state-to-file (concat persp-save-dir "user-layout")))


(defun user-config/startify-home ()
  (require 'startify-home)
  (spacemacs/set-leader-keys (kbd "bH") 'spacemacs/home)
  (spacemacs/set-leader-keys (kbd "bh") 'startify-home/home)
  (setq startify-home-custom-header (shell-command-to-string "fortune | cowsay")))
