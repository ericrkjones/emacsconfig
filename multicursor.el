(require 'multiple-cursors)

;; Sublime-like edit each line of marked region with separate cursor
(global-set-key (kbd "C-S-l") 'mc/edit-lines)

;; Mark next/previous like this
;; This should be replaced with a method that accepts regular expressions!
(global-set-key (kbd "C->") 'mc/mark-next-like-this)
(global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
(global-set-key (kbd "C-M-<") 'mc/mark-all-like-this)

;; Command to exit multiple cursors (duplicate of C-g)
(define-key mc/keymap (kbd "C-<escape>") 'keyboard-quit)

;; Do not exit with return key
(define-key mc/keymap (kbd "<return>") nil)

;; Mouse bindings
(global-unset-key (kbd "C-<down-mouse-1>"))
(global-set-key (kbd "C-<down-mouse-1>") 'mc/add-cursor-on-click)
