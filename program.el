
(defun user-config/program ()
  (user-config/programing-base)
  (user-config/helm-dash)
  (user-config/snippets)
  (user-config/imenu)

  (add-hook 'scheme-mode-hook #'evil-cleverparens-mode)
  (add-hook 'emacs-lisp-mode-hook #'evil-cleverparens-mode))

(defun user-config/programing-base ()
  (setq c-basic-offset 4)
  (setq c++-basic-offset 4)
  (setq clang-format-executable "clang-format-3.8")

  (dolist (mode '(c-mode c++-mode))
    (spacemacs/set-leader-keys-for-major-mode mode "=" 'clang-format-buffer))

  (setq flycheck-clang-args "-std=c99")
  (setq flycheck-gcc-args "-std=c99")

  ;; (evil-define-key 'insert web-mode-map (kbd "TAB") 'emmet-expand-yas)
  ;; (define-key evil-insert-state-map (kbd "TAB") 'user-config/tab-indent-or-complete)
  ;; (dolist (mode-map '(c-mode-map c++-mode-map python-mode-map))
  ;;   (evil-define-key 'insert mode-map (kbd "TAB") 'user-config/tab-indent-or-complete))
  (add-hook 'c-mode-hook '(lambda () (evil-define-key 'insert c-mode-map (kbd "TAB") 'user-config/tab-indent-or-complete)))
  (add-hook 'c++-mode-hook '(lambda () (evil-define-key 'insert c++-mode-map (kbd "TAB") 'user-config/tab-indent-or-complete)))
  (add-hook 'python-mode-hook '(lambda () (evil-define-key 'insert python-mode-map (kbd "TAB") 'user-config/tab-indent-or-complete)))
  (add-hook 'org-mode-hook '(lambda () (evil-define-key 'insert org-mode-map (kbd "TAB") 'user-config/tab-indent-or-complete)))
  (define-key evil-insert-state-map (kbd "C-j") 'user-function/do-yas-expand)


  (dolist (mode '(c-mode c++-mode))
    (spacemacs/set-leader-keys-for-major-mode mode "c" 'user-config/c-cpp-simple-compile))


  ;; https://github.com/proofit404/anaconda-mode/issues/62
  (when (spacemacs/system-is-linux)
    (setq python-shell-interpreter "/usr/bin/python3") ;; 似乎不行
    ;; 在那个文件夹里做了一个小小的软链接
    (setenv "PATH" (concat "~/.spacemacs.d/others/spacemacs-bash-replace" ":" (getenv "PATH"))))

  ;; 缩写补全 http://emacs.stackexchange.com/questions/2671/how-can-i-get-fuzzy-code-completion
  (with-eval-after-load 'company
    (setq completion-styles (cons 'initials completion-styles)))

  ;; 不启用pylint,这个太严格了
  (setq flycheck-python-pylint-executable "pylint-off")
  )

(defun user-config/c-cpp-simple-compile ()
  (interactive)
  ;; TODO
  (let ((filename (buffer-file-name (current-buffer))))
    (compile
     (format "%s \"%s\" -o \"%s\" -g"
             (if (string= (file-name-extension filename) "c") "gcc" "g++")
             filename
             (concat
              (file-name-directory filename)
              (file-name-base filename)
              (if (spacemacs/system-is-mswindows) ".exe" ".out"))))))

(defun user-config/helm-dash ()
  (let (path)
    ;; (setq path "~/dash-docsets")
    (setq path "/media/removable/SD Card/software-data/zeal/docsets")

    ;; 我也不知道到底哪个有效，干脆全写上
    (setq helm-dash-docsets-path path
          dash-helm-dash-docset-path path
          helm-dash-docset-newpath path))

  (setq helm-dash-browser-func 'eww))

(defun user-config/snippets ()
  ;; (add-to-list 'yas-snippet-dirs "~/.spacemacs.d/yasnippet-vim-snippets/snippets")
  )

(defun user-config/tab-indent-or-complete ()
  (interactive)
  (if (minibufferp)
      (minibuffer-complete)
    (if (or (not yas/minor-mode)
            (null (user-function/do-yas-expand)))
        (if (user-function//check-expansion)
            (company-complete-common)
          (indent-for-tab-command)))))

(defun user-function//check-expansion ()
  (save-excursion
    (if (looking-at "\\_>") t
      (backward-char 1)
      (if (looking-at "\\.") t
        (backward-char 1)
        (if (looking-at "->") t nil)))))

(defun user-function/do-yas-expand ()
  (interactive)
  (let ((yas/fallback-behavior 'return-nil))
    ;; (yas/expand)
    (yas-expand)))



(defun user-config/imenu ()
  (with-eval-after-load 'imenu-list
    (define-key imenu-list-major-mode-map (kbd "TAB") 'imenu-list-display-entry)
    (define-key imenu-list-major-mode-map (kbd "za") 'hs-toggle-hiding)
    (define-key imenu-list-major-mode-map (kbd "zc") 'hs-hide-block)
    (define-key imenu-list-major-mode-map (kbd "zo") 'hs-show-block)
    ;; 不行 那个函数要在被生成imenu的buffer里调用
    ;; (define-key imenu-list-major-mode-map (kbd "u") '(lambda () (interactive) (imenu-list-update)))
    ))
