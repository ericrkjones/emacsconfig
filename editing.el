;; Editor commands

;; Turn off word wrap for programming modes
(add-hook 'prog-mode-hook (lambda () (setq truncate-lines t)))

;; Change C-f and C-S-f to isearch regex forward and backward respectively
(global-set-key (kbd "C-f") 'isearch-forward-regexp)
(global-set-key (kbd "C-S-f") 'isearch-backward-regexp)

;; Change C-s and C-S-s to "save" and "save as" respectively
(global-set-key (kbd "C-s") 'save-buffer)
(global-set-key (kbd "C-S-s") 'write-file)

;; Change C-w to close current buffer
(global-set-key (kbd "C-w") 'kill-this-buffer)
(global-set-key (kbd "C-S-w") 'kill-buffer-and-window)

;; Change C-o to run the find-file interactive Lisp function
(global-set-key (kbd "C-o") 'find-file)

;; Bind C-/ to filetype-specific "comment"
(global-set-key (kbd "C-/") 'comment-line) ; Bind C-/ to insert comment
(global-set-key (kbd "C-?") 'uncomment-line) ; Bind C-? to remove comments

;; Bind C-a to select all
(global-set-key (kbd "C-a") 'mark-whole-buffer)

;; New file creation function bound to C-n
(defun new-file-tmp()
  "Create a new empty file."
  (interactive)
  (let ((buf (generate-new-buffer "untitled")))
(switch-to-buffer buf)
(put 'buffer-offer-save 'permanent-local t)
(setq buffer-offer-save t)))
(defun tool-bar-local-item-pre (icon def key map after_item &rest props)
  "Add an item to the tool bar in map MAP.
ICON names the image, DEF is the key definition and KEY is a symbol
for the fake function key in the menu keymap. Remaining arguments
PROPS are additional items to add to the menu item specification. See
Info node ‘(elisp)Tool Bar’. The item is added after AFTER_ITEM.
ICON is the base name of a file containing the image to use. The
function will first try to use low-color/ICON.xpm if ‘display-color-cells’
is less or equal to 256, then ICON.xpm, then ICON.pbm, and finally
ICON.xbm, using ‘find-image’."
  (let* ((image-exp (tool-bar--image-expression icon)))
(define-key-after map (vector key)
  `(menu-item ,(symbol-name key) ,def :image ,image-exp ,@props) after_item)
  (force-mode-line-update)))
(when (boundp 'tool-bar-map)
  (tool-bar-local-item-pre "new" 'new-file-tmp 'new-file-tmp tool-bar-map
               'new-file :label "" :help "New untitled File")
  (define-key tool-bar-map (vector 'new-file) nil)
  )
(global-set-key (kbd "C-n") 'new-file-tmp)

