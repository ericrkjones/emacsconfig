(require 'org)
(setq org-support-shift-select t)
(setq org-startup-truncate t)

;; Silly duplicate key mapping entry.
;; Fix this by putting all custom keymaps into a minor mode and enabling
;; that minor mode at all times with priority over other modes. 
(define-key org-mode-map (kbd "C-<tab>") 'tabbar-forward)
(define-key org-mode-map (kbd "C-S-<tab>") 'tabbar-backward)
