 (defun user-function/org-ctrl-c-ctrl-c ()
   "增强版的ctrl-c-ctrl-c，对链接会编辑链接，对图片会刷新所有内嵌图片"
   (interactive)
   (cond ((user-function/org-image-p) (org-redisplay-inline-images))
         ((user-function/org-link-p) (org-insert-link))
         (t (org-ctrl-c-ctrl-c))))

 (defun user-function/org-minus ()
   (interactive)
   (cond
    ((org-at-heading-p) (org-todo))
    ((org-at-table-p) (call-interactively 'org-table-insert-hline) (evil-next-line))
    ((org-in-item-p) (call-interactively 'org-cycle-list-bullet))
    (t (call-interactively 'org-toggle-item))))

(defun user-function/org-plus ()
  (interactive)
  (cond
   ((or (org-in-item-p) (org-at-heading-p))
    (user-function/org-toggle-statistics-cookies))))

(defun user-function/org-shift-minus ()
   (interactive)
   (cond ((org-at-timestamp-p t) (org-toggle-timestamp-type))
         ((org-at-heading-p) (org-priority-down))
         ((org-at-item-p) (org-toggle-checkbox '(4)))
         ((org-at-table-p) (org-table-toggle-coordinate-overlays))
         (t nil)))

 (defun user-function/org-return ()
   (interactive)
   (cond ((user-function/org-at-p '(link footnote-definition footnote-reference timestamp)) (org-open-at-point))
         ((org-at-heading-p) (user-function/org-insert-heading (org-entry-is-todo-p)))
         ((org-at-item-p) (user-function/org-insert-item (org-at-item-checkbox-p)))
         ((org-at-table-p) (org-table-insert-row t))
         (t (org-open-at-point))))

(defun user-function/org-shift-return ()
  (interactive)
  (cond ((org-at-heading-p) (user-function/org-insert-heading (org-entry-is-todo-p) 'before))
        ((org-at-item-p) (user-function/org-insert-item (org-at-item-checkbox-p) 'before))))

 (defun user-function/org-arrow-left ()
   (interactive)
   (cond ((org-at-heading-p) (org-do-promote))
         ((org-at-item-p) (org-outdent-item))
         ((org-at-table-p) (org-table-delete-column))
         (t (apply 'evil-shift-left (evil-operator-range)))))

 (defun user-function/org-arrow-right ()
   (interactive)
   (cond ((org-at-heading-p) (org-do-demote))
         ((org-at-item-p) (org-indent-item))
         ((org-at-table-p) (org-table-goto-column (+ 1 (org-table-current-column))) (org-table-insert-column))
         (t (apply 'evil-shift-right (evil-operator-range)))))

 (defun user-function/org-left ()
   (interactive)
   (cond ((org-at-heading-p) (call-interactively 'org-promote-subtree))
         ((org-at-item-p) (call-interactively 'org-outdent-item-tree))
         ((org-at-table-p) (org-table-move-column-left))))

 (defun user-function/org-right ()
   (interactive)
   (cond ((org-at-heading-p) (call-interactively 'org-demote-subtree))
         ((org-at-item-p) (call-interactively 'org-indent-item-tree))
         ((org-at-table-p) (org-table-move-column-right))))


 (defun user-function/org-up ()
   (interactive)
   (if (org-at-timestamp-p t)
       (call-interactively (if org-edit-timestamp-down-means-later
                               'org-timestamp-up 'org-timestamp-down))
     (org-metaup)))


 (defun user-function/org-down ()
   (interactive)
   (if (org-at-timestamp-p t)
       (call-interactively (if org-edit-timestamp-down-means-later
                               'org-timestamp-down 'org-timestamp-up))
     (org-metadown)))


 (evil-define-key 'normal org-mode-map
   "-" 'user-function/org-minus
   "_" 'user-function/org-shift-minus
   "+" 'user-function/org-plus
   (kbd "RET") 'user-function/org-return
   (kbd "<S-return>") 'user-function/org-shift-return
   "[[" 'outline-previous-visible-heading
   "]]" 'outline-next-visible-heading
   "<" 'user-function/org-arrow-left
   ">" 'user-function/org-arrow-right
   (kbd "M-h") 'user-function/org-left
   (kbd "M-l") 'user-function/org-right
   (kbd "M-k") 'user-function/org-up
   (kbd "M-j") 'user-function/org-down)


 ;; 默认的快捷键设置有点浪费键位
 ;; 比如<mleader>c和<leader>aoc与<leader>Cc是一样的
 (spacemacs/declare-prefix-for-mode 'org-mode "mt" "tables")
 (spacemacs/declare-prefix-for-mode 'org-mode "mT" "toggles/todo")
 (spacemacs/declare-prefix-for-mode 'org-mode "mx" "font-styles")
 (spacemacs/declare-prefix-for-mode 'org-mode "mS" "subtree")
 (spacemacs/declare-prefix-for-mode 'org-mode "mi" "insert")
 (spacemacs/declare-prefix-for-mode 'org-mode "mg" "goto")

 (spacemacs/declare-prefix-for-mode 'org-mode "mtt" "toggles") ; 表格前缀的子前缀
 (spacemacs/declare-prefix-for-mode 'org-mode "mti" "insert")
 (spacemacs/declare-prefix-for-mode 'org-mode "mtd" "delete")

 (spacemacs/set-leader-keys-for-major-mode 'org-mode
   "T" nil

   "H" nil
   "J" nil
   "K" nil
   "L" nil

   "h" nil
   "l" nil ; 默认的就是org-open-at-point

   "C-S-l" nil
   "C-S-h" nil
   "C-S-j" nil
   "C-S-k" nil

   "S" nil ; 默认是标题树操作

   "tr" '(lambda () (interactive) (org-table-recalculate 'all))
   "t+" 'org-table-sum

   ;; 默认的T是org-show-todo-tree
   "Ti" 'org-toggle-inline-images
   "Tl" 'org-toggle-link-display
   "Tt" 'org-show-todo-tree

   "," 'user-function/org-ctrl-c-ctrl-c
   "it" '(lambda () (interactive) (org-time-stamp nil))
   "iT" '(lambda () (interactive) (org-time-stamp t))
   ;; 截屏
   "is" 'user-function/org-screenshot)

