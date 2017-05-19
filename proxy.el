(defun user-config/proxy()
  ;; (setq url-proxy-services
  ;; '(("no_proxy" . "^\\(localhost\\|10.*\\)")
  ;; ("http" . "127.0.0.1:8888") ; http 代理 proxy_host 换成代理的域名或 ip，port 换成代理的端口
  ;; ("https" . "127.0.0.1:8888") ;https 代理
  ;; ))

  ;; elpa.emacs-china.org
  (setq configuration-layer--elpa-archives
        '(("melpa-cn" . "http://elpa.zilongshanren.com/melpa/")
          ("org-cn"   . "http://elpa.zilongshanren.com/org/")
          ("gnu-cn"   . "http://elpa.zilongshanren.com/gnu/"))))

