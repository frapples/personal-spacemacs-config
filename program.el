
(defun user-config/program ()
  (user-config/c-c++)
  (user-config/python)
  (user-config/kotlin)
  (user-config/helm-dash)
  (user-config/imenu)
  (user-config/ugly-patch)

  (add-hook 'scheme-mode-hook #'evil-cleverparens-mode)
  (add-hook 'emacs-lisp-mode-hook #'evil-cleverparens-mode))

(defun user-config/c-c++ ()
  (setq c-basic-offset 4)
  (setq c++-basic-offset 4)
  (setq clang-format-executable "clang-format-3.8")

  (dolist (mode '(c-mode c++-mode))
    (spacemacs/set-leader-keys-for-major-mode mode "=" 'clang-format-buffer))

  ;; https://stackoverflow.com/questions/30949847/configuring-flycheck-to-work-with-c11
  (add-hook 'c++-mode-hook
            (lambda ()
              (setq flycheck-clang-language-standard "c++11")
              (setq flycheck-gcc-language-standard "c++11")))

  (add-hook 'c-mode-hook
            (lambda ()
              (setq flycheck-clang-language-standard "c99")
              (setq flycheck-gcc-language-standard "c99")))

  (dolist (mode '(c-mode c++-mode))
    (spacemacs/set-leader-keys-for-major-mode mode "c" 'user-function/c-cpp-simple-compile))
  )

(defun user-config/python ()
  ;; https://github.com/proofit404/anaconda-mode/issues/62
  (when (spacemacs/system-is-linux)
    (setq python-shell-interpreter "/usr/bin/python3")
    ;; 在那个文件夹里做了一个小小的软链接
    ;; (setenv "PATH" (concat "~/.spacemacs.d/others/spacemacs-bash-replace" ":" (getenv "PATH")))
    )

  ;; 缩写补全 http://emacs.stackexchange.com/questions/2671/how-can-i-get-fuzzy-code-completion
  (with-eval-after-load 'company
    (setq completion-styles (cons 'initials completion-styles)))

  ;; 不启用pylint,这个太严格了
  (setq flycheck-python-pylint-executable "pylint-off")
  )

(defun user-config/kotlin ()
  (require 'kotlin-mode)
  (require 'flycheck-kotlin)
  (add-hook 'kotlin-mode-hook 'flycheck-mode)

  (setq kotlin-tab-width 4))

(defun user-config/helm-dash ()
  (let (path)
    ;; (setq path "~/dash-docsets")
    (setq path "/media/removable/SD Card/software-data/zeal/docsets")

    ;; 我也不知道到底哪个有效，干脆全写上
    (setq helm-dash-docsets-path path
          dash-helm-dash-docset-path path
          helm-dash-docset-newpath path))

  (setq helm-dash-browser-func 'eww))

(defun user-config/imenu ()
  (with-eval-after-load 'imenu-list
    (define-key imenu-list-major-mode-map (kbd "TAB") 'imenu-list-display-entry)
    (define-key imenu-list-major-mode-map (kbd "za") 'hs-toggle-hiding)
    (define-key imenu-list-major-mode-map (kbd "zc") 'hs-hide-block)
    (define-key imenu-list-major-mode-map (kbd "zo") 'hs-show-block)
    ;; 不行 那个函数要在被生成imenu的buffer里调用
    ;; (define-key imenu-list-major-mode-map (kbd "u") '(lambda () (interactive) (imenu-list-update)))
    ))

(defun user-config/ugly-patch ()
  ;; https://emacs.stackexchange.com/questions/30082/your-python-shell-interpreter-doesn-t-seem-to-support-readline
  (with-eval-after-load 'python
    (defun python-shell-completion-native-try ()
      "Return non-nil if can trigger native completion."
      (let ((python-shell-completion-native-enable t)
            (python-shell-completion-native-output-timeout
             python-shell-completion-native-try-output-timeout))
        (python-shell-completion-native-get-completions
         (get-buffer-process (current-buffer))
         nil "_")))))


(defun user-function/c-cpp-simple-compile ()
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
