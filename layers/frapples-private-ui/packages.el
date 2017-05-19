(defconst frapples-private-ui-packages
  '(
    all-the-icons
    (all-the-icons :location elpa)
    all-the-icons-dired
    (all-the-icons-dired :location elpa)

    spaceline-all-the-icons
    (spaceline-all-the-icons :location elpa)

    spaceline
    ))


;; Thanks to https://emacs-china.org/t/neotree-speedbar/2364/13
(defun frapples-private-ui/init-all-the-icons ()
  (use-package all-the-icons)

  (with-eval-after-load 'neotree
    (setq neo-theme (if (display-graphic-p) 'icons 'arrow))))


(defun frapples-private-ui/init-all-the-icons-dired ()
  (use-package all-the-icons-dired)
  ;; https://github.com/ralesi/ranger.el/issues/136
  (add-hook 'ranger-mode-hook 'all-the-icons-dired-mode)
  (add-hook 'dired-mode-hook 'all-the-icons-dired-mode))


(defun frapples-private-ui/init-spaceline-all-the-icons ()
  ;; 太卡了，给关了
  ;; (use-package spaceline-all-the-icons
  ;;   :after spaceline
  ;;   :config (spaceline-all-the-icons-theme))

  ;; (spaceline-all-the-icons--setup-package-updates)
  ;; (spaceline-all-the-icons--setup-neotree)
  ;; (spaceline-all-the-icons--setup-paradox)

  ;; (setq spaceline-all-the-icons-hide-long-buffer-path t)

  ;; (setq spaceline-all-the-icons-icon-set-modified 'circle)

  ;; ;; https://github.com/domtronn/spaceline-all-the-icons.el/issues/32#issuecomment-301261557
  ;; (spaceline-all-the-icons-theme 'buffer-encoding-abbrev 'line-column)

  ;; ;; (setq powerline-text-scale-factor 1.1)
  )


(defun frapples-private-ui/post-init-spaceline ()
  ;; powerline分割符。有时没效果，加上(spaceline-compile)解决
  (setq powerline-default-separator 'arrow) ; 其它的比如：arrow curve

  ;; 默认只显示buffer-encoding-abbrev，我把这个段重新定义成完全编码，这样就能显示完全编码了
  (spaceline-define-segment buffer-encoding-abbrev
    "The full `buffer-file-coding-system'."
    (format "%s" buffer-file-coding-system))
  ;; 重定义后需要重新compile (https://github.com/TheBB/spaceline#segments)
  (spaceline-compile))
