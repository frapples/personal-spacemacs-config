(defun user-function/evil-paste-after-from-register-0 ()
  "粘贴寄存器0，即上次yank的内容而非delete和change的内容"
  (interactive)
  (evil-paste-after 1 ?0))

(spacemacs/set-leader-keys "bc" 'spacemacs/kill-this-buffer)
(define-key evil-normal-state-map "gp" 'user-function/evil-paste-after-from-register-0)
(define-key evil-motion-state-map (kbd "g b") 'ace-jump-buffer)
(global-set-key [f11] 'spacemacs/toggle-fullscreen-frame)
(define-key evil-normal-state-map (kbd "C-a") 'evil-numbers/inc-at-pt)
(define-key evil-normal-state-map (kbd "C-x") 'evil-numbers/dec-at-pt)

;; 普通模式的ESC取消搜索的高亮
(define-key evil-normal-state-map
  [escape] '(lambda () (interactive) (evil-search-highlight-persist-remove-all) (evil-normal-state)))

(spacemacs/set-leader-keys
  "fCe" 'set-buffer-file-coding-system
  "fCr" 'revert-buffer-with-coding-system)



(defun user-function/temp-prefix ()
  (interactive))
(define-prefix-command 'user-function/temp-prefix)
(define-key Info-mode-map (kbd "g") 'user-function/temp-prefix)
(define-key Info-mode-map (kbd "gg") 'evil-goto-first-line)
(define-key Info-mode-map (kbd "G") 'evil-goto-line)
(spacemacs/set-leader-keys-for-major-mode 'Info-mode "g" 'Info-goto-node)



;; 使用eyebrowse实现vim的tab概念
(define-key evil-motion-state-map (kbd "g TAB") 'eyebrowse-last-window-config)


(evil-define-command user-function/evil-tabclose ()
  "关闭eyebrowse的工作区，并且重新索引编号"
  (interactive)
  (eyebrowse-close-window-config)
  (user-function/eyebrowse-reindex-slot)
  (user-function/eyebrowse-update-frame-title))

(evil-define-command user-function/evil-tabnew (&optional file)
  :repeat nil
  (interactive "<f>")
  (eyebrowse-switch-to-window-config (+ (user-function/eyebrowse-max-slot-number) 1))
  (delete-other-windows)
  (if file
      (evil-edit file)
    (evil-buffer-new 1 nil))
  (user-function/eyebrowse-update-frame-title))

(evil-define-command user-function/evil-quit (&optional force)
  :repeat nil
  (interactive "<!>")
  (let (window-count)
    (setq window-count
          (length (spacemacs/window-state-get-buffer-names (window-state-get))))

    (if (or (<= (user-function/eyebrowse-slot-count) 1)
            (> window-count 1))
        (evil-quit force)
      (user-function/evil-tabclose))))

(push '("tabc" . user-function/evil-tabclose) evil-ex-commands)
(push '("tabclose" . user-function/evil-tabclose) evil-ex-commands)
(push '("tabn" . user-function/evil-tabnew) evil-ex-commands)
(push '("tabnew" . user-function/evil-tabnew) evil-ex-commands)
(push '("q" . user-function/evil-quit) evil-ex-commands)
(push '("quit" . user-function/evil-quit) evil-ex-commands)
