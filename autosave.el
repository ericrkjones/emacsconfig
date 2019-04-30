;; emacs autosave and file backup functionality.
;; Thanks to Andreas Spindler and ntc2 for their great advice:
;; https://stackoverflow.com/a/18330742/5227611
;; https://stackoverflow.com/a/20824625/5227611

;; Backup and autosave functionality should only be applied to files in the group
;; that the user works with.
;; A single backup for the purpose of undoing mistakes should be kept in the same
;; directory as the file.
;; It shall be overwritten whenever the user saves.  This functionality could be
;; expanded upon for more sensitive files, but in general this is the
;; functionality I want.
;; Additionally, every time that a timer elapses or the user inputs a significant
;; portion of keystrokes without saving, an autosave feature should kick in and
;; proactively guard against data loss.  The autosaved files should be with the
;; backup files.  

;; This is for putting all backup files in a specific directory.
;; (defvar user-emacs-backup-directory (concat user-emacs-directory "backup"))
;; (if (not (file-exists-p user-emacs-backup-directory))
;;     (make-directory user-emacs-backup-directory))
;; (setq backup-directory-alist `((".*" . , user-emacs-backup-directory)))

;; This is for putting all autosave files in a specific directory.  
;; (defvar user-emacs-auto-save-directory (concat user-emacs-directory "autosave"))
;; (if (not (file-exists-p user-emacs-auto-save-directory))
;;     (make-directory user-emacs-auto-save-directory))
;; (setq auto-save-file-name-transforms `((".*" user-emacs-auto-save-directory t)))

(defun force-backup-of-buffer ()
  (interactive)
  ;; Make a backup of the buffer.  This should be done on each save.
  (let ((buffer-backed-up nil))
    (backup-buffer)))

(setq backup-by-copying t           ; Do not clobber symlinks
      )
(setq auto-save-default t           ;
      auto-save-timeout 15          ;
      auto-save-interval 150        ;
      )
(add-hook 'before-save-hook 'force-backup-of-buffer)
