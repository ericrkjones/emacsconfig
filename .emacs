;;
;;MELPA Repository Configuration
;;
(require 'package)
(let* ((no-ssl (and (memq system-type '(windows-nt ms-dos))
                    (not (gnutls-available-p))))
       (proto (if no-ssl "http" "https")))
  (when no-ssl
    (warn "\
Your version of Emacs does not support SSL connections,
which is unsafe because it allows man-in-the-middle attacks.
There are two things you can do about this warning:
1. Install an Emacs version that does support SSL and be safe.
2. Remove this warning from your init file so you won't see it again."))
  ;; Comment/uncomment these two lines to enable/disable MELPA and MELPA Stable as desired
  (add-to-list 'package-archives (cons "melpa" (concat proto "://melpa.org/packages/")) t)
  ;;(add-to-list 'package-archives (cons "melpa-stable" (concat proto "://stable.melpa.org/packages/")) t)
  (when (< emacs-major-version 24)
    ;; For important compatibility libraries like cl-lib
    (add-to-list 'package-archives (cons "gnu" (concat proto "://elpa.gnu.org/packages/")))))
(package-initialize)

;; Create isearch wrapping with feedback
(defadvice isearch-repeat (after isearch-no-fail activate)
  (unless isearch-success
    (ad-disable-advice 'isearch-repeat 'after 'isearch-no-fail)
    (ad-activate 'isearch-repeat)
    (isearch-repeat (if isearch-forward 'forward))
    (ad-enable-advice 'isearch-repeat 'after 'isearch-no-fail)
    (ad-activate 'isearch-repeat)))

;; Change C-f and C-S-f to isearch regex forward and backward respectively
(global-set-key (kbd "C-f") 'isearch-forward-regexp)
(global-set-key (kbd "C-S-f") 'isearch-backward-regexp)

;; Change C-s and C-S-s to "save" and "save as" respectively
(global-set-key (kbd "C-s") 'save-buffer)
(global-set-key (kbd "C-S-s") 'write-file)

;; Change C-w to close current buffer
(global-set-key (kbd "C-w") 'kill-this-buffer)

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


;; Change yes/no questions to y/n questions by default
(defalias 'yes-or-no-p 'y-or-n-p)

;; Create a temporary shortcut to reload this config
(defun load-emacs()
  "Loads the user's default emacs configuration file."
  (interactive)
  (load-file "~/.emacs")
)

; START TABS CONFIG
;; Create a variable for our preferred tab width
(setq custom-tab-width 4)

;; Two callable functions for enabling/disabling tabs in Emacs
(defun disable-tabs () (setq indent-tabs-mode nil))
(defun enable-tabs  ()
  (local-set-key (kbd "TAB") 'tab-to-tab-stop)
  (setq indent-tabs-mode t)
  (setq tab-width custom-tab-width)
)
;; Hooks to Enable Tabs
(add-hook 'prog-mode-hook 'enable-tabs)
;; Hooks to Disable Tabs
(add-hook 'lisp-mode-hook 'disable-tabs)
(add-hook 'emacs-lisp-mode-hook 'disable-tabs)

;; Language-Specific Tweaks
(setq-default python-indent-offset custom-tab-width) ;; Python
(setq-default js-indent-level custom-tab-width)      ;; Javascript

;; Making electric-indent behave sanely
(setq-default electric-indent-inhibit t)

;; Make the backspace properly erase the tab instead of
;; removing 1 space at a time.
(setq backward-delete-char-untabify-method 'hungry)

;; ;; WARNING: This will change your life
;; ;; (OPTIONAL) Visualize tabs as a pipe character - "|"
;; ;; This will also show trailing characters as they are useful to spot.
;; (setq whitespace-style '(face tabs tab-mark trailing))
;; (custom-set-faces
;;  '(whitespace-tab ((t (:foreground "#636363")))))
;; (setq whitespace-display-mappings
;;   '((tab-mark 9 [124 9] [92 9]))) ; 124 is the ascii ID for '\|'
;; (global-whitespace-mode) ; Enable whitespace mode everywhere
; END TABS CONFIG

;; Configure Tab Bar
(defun my-tabbar-buffer-groups () ;; customize to show all normal files in one group
"Returns the name of the tab group names the current buffer belongs to.
  There are two groups: Emacs buffers (those whose name starts with '*', plus
  dired buffers), and the rest.  This works at least with Emacs v24.2 using
  tabbar.el v1.7."
(list (cond ((string-equal "*" (substring (buffer-name) 0 1)) "emacs")
            ((eq major-mode 'dired-mode) "emacs")
            (t "user"))))
(setq tabbar-buffer-groups-function 'my-tabbar-buffer-groups)

;; Bind C-tab and C-S-tab to next and previous buffer respectively
;;(global-set-key (kbd "<C-tab>") 'next-buffer) ;; Without tab bar
(global-set-key (kbd "<C-tab>") 'tabbar-backward-tab) ;; With tab bar

;; Works as long as there is no "righttab" to cover as well
;; (global-set-key (kbd "<C-iso-lefttab>") 'previous-buffer) ;; Without tab bar
(global-set-key (kbd "<C-tab>") 'tabbar-forward-tab) ;; With tab bar


;; Clean up the tabbar settings so that tabbar-selected and unselected inherit tabbar-button for box, color, background color.  Same for modified tabs.
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
