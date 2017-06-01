(defconst frapples-private-evil-extends-packages
  '(
    ;; Get the package from MELPA, ELPA, etc.
    evil-find-char-pinyin
    (evil-find-char-pinyin :location elpa)

    evil-vimish-fold
    (evil-vimish-fold :location elpa)

    ace-jump-buffer
    (ace-jump-buffer :location elpa)

    avy
    (avy :location elpa)

    pangu-spacing
    evil-visual-mark-mode
    evil-cleverparens
    eyebrowse
    ranger

    ;; gtags等层依赖这个helm
    helm
    ))

;; https://github.com/syl20bnr/spacemacs/issues/7713
;; https://github.com/syl20bnr/spacemacs/issues/6121
;; 部分层的功能依赖helm
(defun frapples-private-evil-extends/init-helm ()
  (use-package helm
    :defer 1
    :init
    :config
    (progn
      (helm-mode)
      (with-eval-after-load 'helm-mode ; required
        (spacemacs|hide-lighter helm-mode)))))

(defun frapples-private-evil-extends/init-evil-find-char-pinyin ()
  (use-package evil-find-char-pinyin)
  (evil-find-char-pinyin-mode +1)
  )

(defun frapples-private-evil-extends/init-evil-vimish-fold ()
  (use-package evil-vimish-fold)
  ;; (evil-vimish-fold-mode 1)
  )

(defun frapples-private-evil-extends/init-ace-jump-buffer ()
  (use-package ace-jump-buffer)
  )



(defun frapples-private-evil-extends/post-init-avy ()
  ;; 默认的跳转就几个字符能用
  (setq avy-keys '(?q ?w ?e ?r ?t ?y ?u ?i ?o ?p ?a ?s ?d ?f ?g ?h ?j ?k ?l ?\; ?z ?x ?c ?v ?b ?n ?m)))

(defun frapples-private-evil-extends/post-init-evil-visual-mark-mode ()
  ;; 解决evil-delete-marks
  ;; https://emacs-china.org/t/evil-delete-marks/1358
  (evil-visual-mark-mode))

(defun frapples-private-evil-extends/post-init-evil-cleverparens ()
  (require 'evil-cleverparens-text-objects
   (setq evil-cleverparens-use-additional-movement-keys nil
         evil-cleverparens-move-skip-delimiters nil)

   (spacemacs/toggle-evil-cleverparens-on)))


(defun frapples-private-evil-extends/post-init-eyebrowse ()
  (add-hook 'eyebrowse-post-window-switch-hook 'user-function/eyebrowse-update-frame-title)
  ;; 查文档查不到当(select-window)发生时调用的hook。最后查(select-window)的文档才得知
  (add-hook 'buffer-list-update-hook 'user-function/eyebrowse-update-frame-title))

(defun frapples-private-evil-extends/post-init-ranger ()
  ;; 修复ranger的一个问题
  ;; https://emacs-china.org/t/ranger-void-variable-bookmark-list-ranger-pre-header-format/2179
  (require 'bookmark))

(defun chinese/post-init-pangu-spacing ()
  ;; 把chinese的这个设置关掉，有bug：https://github.com/coldnew/pangu-spacing/issues/23
  (add-hook 'org-mode-hook
            '(lambda ()
               (set (make-local-variable 'pangu-spacing-real-insert-separtor) nil))
            t))
