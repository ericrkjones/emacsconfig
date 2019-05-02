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

;; Rectangular region mark mode
(global-set-key (kbd "C-M-<SPC>") 'set-rectangular-region-anchor)

;; Command to exit multiple cursors (duplicate of C-g)
(define-key mc/keymap (kbd "C-<escape>") 'keyboard-quit)

;; Do not exit with return key
(define-key mc/keymap (kbd "<return>") nil)

;; Mouse bindings
(global-unset-key (kbd "C-<down-mouse-1>"))
(global-set-key (kbd "C-<down-mouse-1>") 'mc/add-cursor-on-click)

(defun mouse-start-rectangular-region (click)
  "Start the selection of a rectangular region by setting the 
point with the mouse and then setting the rectangular region anchor."
  (interactive "e")
  (mouse-set-point click)
  (set-rectangular-region-anchor))


(defun mouse-set-rectangular-region (click)
  "Set the rectangular region to the text the mouse is dragged over.  
Do not copy to kill ring regardless of what mouse-drag-copy-region says.
This should be bound to a mouse drag event like."
  (interactive "e")
  (mouse-minibuffer-check click)
  (select-window (posn-window (event-start click)))
  (let ((beg (posn-point (event-start click)))
        (end
         (if (eq (posn-window (event-end click)) (selected-window))
             (posn-point (event-end click))
           ;; If the mouse ends up in any other window or on the menu
           ;; bar, use 'window-point' of selected window (mouse-set-region bugfix).
           (window-point)))
        (click-count (event-click-count click)))
    (let ((drag-start (terminal-parameter nil 'mouse-drag-start)))
      (when drag-start
        ;; Drag events don't come with a click count, so we hack
        ;; our way around the problem by remembering the start-event
        ;; in 'mouse-drag-start' and fetching the click-count from there.
        (when (and (<= click-count 1)
                   (equal beg (posn-point (event-start drag-start))))
          (setq click-count (event-click-count drag-start)))
        ;; ;; Bugfix from mouse.el:
        ;; ;; Occasionally we get spurious drag events where the user hasn't
        ;; ;; dragged his mouse, but instead Emacs has dragged the text under the
        ;; ;; mouse.  Try to recover those cases:
        ;; (when (and (equal (posn-x-y (event-start click))
        ;;                   (posn-x-y (event-end click)))
        ;;            (not (eq (car drag-start) 'mouse-movement)))
        ;;   (setq end beg))
        (setf (terminal-parameter nil 'mouse-drag-start) nil)))
    ;; (when (and (integerp beg) (integerp end))
    ;;   (let ((range (mouse-start-end beg end (1- click-count))))
    ;;     (if (< end beg)
    ;;         (setq end (nth 0 range) beg (nth 1 range))
    ;;       (setq beg (nth 0 range) end (nth 1 range)))))
    (if (numberp beg) (goto-char beg))
    ;; On a text terminal, bounce the cursor.
    (or transient-mark-mode
        (window-system)
        (sit-for 1))
    (push-mark)
    (if (numberp end) (goto-char end))
    (mouse-set-rectangular-region-1)
    ))
    
(defun mouse-set-rectangular-region-1 ()
  ;; Set transient-mark-mode for a little while.
  (unless (eq (car-safe transient-mark-mode) 'only)
    (setq-local transient-mark-mode
                (cons 'only
                      (unless (eq transient-mark-mode 'lambda)
                        transient-mark-mode))))
  ;; (setq mouse-last-region-beg (region-beginning))
  ;; (setq mouse-last-region-end (region-end))
  ;; (setq mouse-last-region-tick (buffer-modified-tick))
    
  )

(global-set-key [C-S-down-mouse-1] 'mouse-start-rectangular-region)
(global-set-key [C-S-mouse-1] 'mouse-set-point)
(global-set-key [C-S-drag-mouse-1] 'mouse-set-rectangular-region)
