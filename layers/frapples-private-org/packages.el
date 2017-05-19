(defconst frapples-private-org-packages
  '(
    ;; Get the package from MELPA, ELPA, etc.
    org-ehtml
    (org-ehtml :location elpa)

    org-download
    deft

    ))

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
