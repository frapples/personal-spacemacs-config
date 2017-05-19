(defun user-config/spacemacs-os ()
  ;; (if (spacemacs/system-is-linux)
  ;;     (user-config/exwm))

  (user-config/input-method)
  ;; (user-config/emms)
    )

(defun user-config/exwm ()

  ;; 需要这个层：https://github.com/ch11ng/exwm
  ;; 当然，我已经不用了，没太大用
  (exwm-enable)
  (define-key exwm-mode-map "A-m" spacemacs-cmds)

  ;; (require 'exim)
  ;; (exim-start)
  )

(defun user-config/input-method ()
  (setq pyim-use-tooltip 'pos-tip)
  ; t 'nil 'pos-til
  ; https://github.com/CestDiego/spacemacs_conf
  ;; (add-hook 'exwm-init-hook 'exim-start)
  ;; (push ?\C-\\ exwm-input-prefix-keys)
  )



(defun user-config/emms ()
  (use-package emms-setup
    :defer t
    :commands (emms-play-directory
               emms-playlist-mode-go
               emms
               emms-play-dired))

  (with-eval-after-load 'emms-setup
    (emms-all)
    (emms-default-players)

    (setq emms-player-list '(emms-player-mplayer)
          emms-player-mplayer-command-name "mplayer"
          emms-player-mplayer-parameters '("-slave"))

    (setq emms-track-description-function '(lambda (track) (file-name-base (emms-track-simple-description track))))

    (setq emms-repeat-playlist t
          emms-source-file-default-directory (expand-file-name "~/music/")
          emms-lyrics-dir (expand-file-name "~/music/")
          emms-lyrics-coding-system nil ;;let emacs to identify the encode of lyrics
          )
    ;; show info at mode-line
    (require 'emms-mode-line)
    (emms-mode-line 1)
    ;; show time of music
    (require 'emms-playing-time)
    (emms-playing-time 1)

    ;; show lyrics
    ;;(require 'emms-lyrics)
    ;;(emms-lyrics 1)
    ;; auto identify encode
    ;;(require 'emms-i18n)

    ;; auto save and import playlist
    ;;(require 'emms-history)
    ;;(emms-history-load)

    (evilified-state-evilify-map emms-playlist-mode-map
      :mode emms-playlist-mode
      :bindings
      (kbd "t") '(lambda () (message "test")))

    (user-config/emms-define-micro-state))

  ;;设置按键绑定
  (spacemacs/set-leader-keys
    (kbd "amP") 'emms-play-directory
    (kbd "amp") 'emms
    (kbd "amm") 'emms-playlist-mode-go-popup
    (kbd "amc") 'spacemacs/emms-micro-state
    (kbd "amh") 'emms-volume-lower
    (kbd "aml") 'emms-volume-raise
    (kbd "amd") 'emms-play-dired
    ))



(defun user-config/emms-define-micro-state ()
    (defun user-function/emms-seek-10 () (interactive) (emms-seek -10))
    (defun user-function/emms-seek+10 () (interactive) (emms-seek +10))
    (defun user-function/noting () (interactive))
    (defun user-function/emms-next ()
      (interactive)
      (if emms-random-playlist
          (funcall emms-player-next-function)
        (emms-next)))
    (defun user-function/emms-previous ()
      (interactive)
      (if emms-random-playlist
          (funcall emms-player-next-function)
        (emms-previous)))

    (defun user-function/emms-current-playing-mode ()
      (cond (emms-repeat-track 'repeat)
            (emms-random-playlist 'random)
            (emms-repeat-playlist 'circle)
            (t 'sequence)))

    (defun user-function/emms-cycle-playing-mode ()
      (interactive)
      (case (user-function/emms-current-playing-mode)
        ('repeat
         (when emms-repeat-track (emms-toggle-repeat-track))
         (unless emms-random-playlist (emms-toggle-random-playlist)))
        ('random
         (when emms-repeat-track (emms-toggle-repeat-track))
         (when emms-random-playlist (emms-toggle-random-playlist))
         (unless emms-repeat-playlist (emms-toggle-repeat-playlist)))
        ('circle
         (when emms-repeat-playlist (emms-toggle-repeat-playlist)))
        ('sequence
         (unless emms-repeat-track (emms-toggle-repeat-track)))))

    (defun user-function/emms-ms-doc ()
      (message "当前播放: %s 模式: %s"
               (if emms-player-playing-p
                   (file-name-base
                    (emms-track-description
                     (emms-playlist-current-selected-track)))
                 "无")
               (case (user-function/emms-current-playing-mode)
                 ('repeat "单曲循环")
                 ('random "随机播放")
                 ('circle "列表循环")
                 ('sequence "顺序播放")))
      "[h]volume-- [j]next [k]previous [l]volume++  [t]cycle mode [p]pause [s]stop"
      )

    (spacemacs|define-micro-state emms
      :doc (user-function/emms-ms-doc)
      ;; :use-minibuffer t
      ;; :disable-evil-leader t
      :bindings
      ("h" emms-volume-lower)
      ("l" emms-volume-raise)
      ("j" user-function/emms-next)
      ("k" user-function/emms-previous)
      ("p" emms-pause)
      ("s" emms-stop :exit t)
      ("S" emms-start)
      ("<" emms-seek-backward)
      (">" emms-seek-forward)
      ("q" user-function/noting :exit t)
      ("<esc>" user-function/noting :exit t)
      ("t" user-function/emms-cycle-playing-mode)))
