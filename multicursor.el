(require 'multiple-cursors)

;; There might be a way to fix the off-screen cursor issue that frustrates
;; multiple-cursor mode.  If you were to replace the main cursor/point with
;; a virtual cursor and place the main cursor in a jailed buffer or something
;; so that its input didn't affect the open buffer, you could keep the
;; functionality of multiple-cursors where the virtual cursors can be scrolled
;; past the edges of the window.

;; Wouldn't it make more sense for that to be a function of the main cursor, though?

;; Sublime-like edit each line of marked region with separate cursor
(global-set-key (kbd "C-S-l") 'mc/edit-lines)

;; Mark next/previous like this
;; This should be replaced with a method that accepts regular expressions!
(global-set-key (kbd "C->") 'mc/mark-next-like-this)
(global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
(global-set-key (kbd "C-M-a") 'mc/mark-all-like-this)
(global-set-key (kbd "C-S-l") 'mc/edit-lines)

;; Command to exit multiple cursors (duplicate of C-g)
(define-key mc/keymap (kbd "C-<escape>") 'keyboard-quit)

;; Do not exit with return key
(define-key mc/keymap (kbd "<return>") nil)

;; Mouse bindings
(global-unset-key (kbd "C-<down-mouse-1>"))
(global-set-key (kbd "C-<down-mouse-1>") 'mc/add-cursor-on-click)
