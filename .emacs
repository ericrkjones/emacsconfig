;; MELPA repository
(load-file "packagerepository.el")


;; Change yes/no questions to y/n questions by default
(defalias 'yes-or-no-p 'y-or-n-p)

;; Create a temporary shortcut to reload this config
(defun load-emacs()
  "Loads the user's default emacs configuration file."
  (interactive)
  (load-file "~/.emacs")
)

;; Text editing
(load-file "editing.el")

;; Indentation
(load-file "indentation.el")

;; Tab bar
(load-file "tabbar.el")

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(cua-mode t nil (cua-base))
 '(cursor-type (quote bar))
 '(custom-enabled-themes (quote (wombat)))
 '(package-selected-packages
   (quote
    (tabbar multiple-cursors auto-complete adaptive-wrap)))
 '(show-paren-mode t)
 '(tabbar-background-color "gray14")
 '(tabbar-mode t nil (tabbar))
 '(tabbar-scroll-left-button (quote (("") "")))
 '(tabbar-scroll-right-button (quote (("") "")))
 '(tool-bar-mode nil)
 '(tooltip-mode nil))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(tabbar-button ((t (:inherit tabbar-default :box (:line-width 1 :color "dim gray")))))
 '(tabbar-default ((t (:inherit variable-pitch :background "gray14" :foreground "dim gray" :height 0.8))))
 '(tabbar-modified ((t (:inherit tabbar-default :foreground "orange red" :box (:line-width 1 :color "dim gray")))))
 '(tabbar-selected ((t (:inherit tabbar-default :foreground "white smoke" :box (:line-width 1 :color "dim gray")))))
 '(tabbar-selected-modified ((t (:inherit tabbar-default :foreground "dark orange" :box (:line-width 1 :color "dim gray")))))
 '(tabbar-unselected ((t (:inherit tabbar-default :box (:line-width 1 :color "dim gray"))))))
