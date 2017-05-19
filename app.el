(defun user-config/app ()

  (spacemacs/set-leader-keys "aw" 'user-config/goldendict-region-or-word)
  ;; elfeed，设置默认过滤器
  ;; (setq-default elfeed-search-filter "@1-year-ago +unread ")

  (user-config/google-translate)
  )


(defun user-config/goldendict-region-or-word ()
  (interactive)
  (call-process "goldendict" nil nil nil (user-config/region-or-word))
  ;; (shell-command (concat "goldendict " (user-config/region-or-word)))
  )

; https://github.com/xuchunyang/youdao-dictionary.el
(defun user-config/region-or-word ()
  "Return word in region or word at point."
  (if (use-region-p)
      (buffer-substring-no-properties (region-beginning)
                                      (region-end))
    (thing-at-point 'word t)))


(defun user-config/google-translate ()
  (setq google-translate-base-url "http://translate.google.cn/translate_a/single")
  (setq google-translate-listen-url "http://translate.google.cn/translate_tts")
  (setq google-translate--tkk-url "http://translate.google.cn/")

  (setq google-translate-default-source-language nil)
  (setq google-translate-default-target-language nil)

  ;; C-n C-p切换不同的规则
  (setq google-translate-translation-directions-alist
        ;; '(("en" . "zh-CN") ("zh-CN" . "en") ("zh-CN" . "zh-TW") ("zh-TW" . "zh-CN"))
        '(("auto" . "zh-CN") ( "auto" . "en") ("auto" . "zh-TW")))

  (spacemacs/set-leader-keys (kbd "a t") 'google-translate-smooth-translate)
  )
