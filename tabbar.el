;; Configure Tab Bar
(defun my-tabbar-buffer-groups () ;; customize to show all normal files in one group
"Returns the name of the tab group names the current buffer belongs to.
  There are two groups: Emacs buffers (those whose name starts with '*', plus
  dired buffers), and the rest.  This works at least with Emacs v24.2 using
  tabbar.el v1.7."
(list (cond ((string-equal "*" (substring (buffer-name) 0 1)) "emacs")
            ((eq major-mode 'dired-mode) "emacs")
            ((eq major-mode 'org-mode) "user")
            (t "user"))))
(setq tabbar-buffer-groups-function 'my-tabbar-buffer-groups)

;; Bind C-tab and C-S-tab to next and previous buffer respectively
;;(global-set-key (kbd "<C-tab>") 'next-buffer) ;; Without tab bar
(global-set-key (kbd "<C-tab>") 'tabbar-forward-tab) ;; With tab bar

;; Works as long as there is no "righttab" to cover as well
;; (global-set-key (kbd "<C-iso-lefttab>") 'previous-buffer) ;; Without tab bar
(global-set-key (kbd "<C-iso-lefttab>") 'tabbar-backward-tab) ;; With tab bar


;; Clean up the tabbar settings so that tabbar-selected and unselected inherit tabbar-button for box, color, background color.  Same for modified tabs.


