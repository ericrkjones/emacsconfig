(require 'org)
(setq org-support-shift-select t)
(setq org-startup-truncated t)

;; Silly duplicate key mapping entry.
;; Fix this by putting all custom keymaps into a minor mode and enabling
;; that minor mode at all times with priority over other modes. 
(define-key org-mode-map (kbd "<C-tab>") 'tabbar-forward-tab)
(define-key org-mode-map (kbd "<C-iso-leftab>") 'tabbar-backward-tab)

;; Hook to add visual line with visual-line-mode breaks tables
;;(add-hook 'org-mode-hook 'visual-line-mode 1)

;; Define functions for export from org and email.
;; Evolution CLI does not seem to support putting a complex body in an email in any way,
;; Including opening a file from disk.  So the stupid solution is to just attach the
;; file to the email instead of making the email from the file.
(defvar org-email-cleanup t)
(setq org-html-postamble nil)
(defun org-email-attachment-html (subject)
  "Compile the org file to an html file in its directory, then open the default email editor to send that file as an attachment."
  (interactive "MSubject:")
  (shell-command (concat "exo-open "
                         (shell-quote-argument (concat "mailto:?subject="
                                                       (url-encode-url subject)
                                                       "&attach="
                                                       (file-name-nondirectory (org-html-export-to-html))
                                                       "&body="
                                                       (url-encode-url (concat "My notes from our meeting \"" subject "\" are attached.\n\nThank you,\nEric Jones")))))))

;; (defun org-email-attachment-pdf (subject)
;;   "Compile the org file to an html file in its directory, then open the default email editor to send that file as an attachment."
;;   (interactive "MSubject:")
;;   (shell-command (concat "exo-open "
;;                          (shell-quote-argument (concat "mailto:?subject="
;;                                                        (url-encode-url subject)
;;                                                        "&attach="
;;                                                        (file-name-nondirectory (org-html-export-to-html)))))))


(defvar org-meeting-notes-directory "~/Notes/org/Meetings")
(defun org-new-meeting-notes (title people)
  "Create a new file for meeting notes and insert typical header information."
  (interactive "MTitle:\nMCSV List of Participants:")
  (find-file (concat (file-name-as-directory org-meeting-notes-directory)
                                    (concat (get-current-date)
                                            " "
                                            title
                                            ".org")))
  (insert (concat "#+Title: " title "\n"
                  "#+Date: " (get-current-datetime) "\n"
                  "#+People: " (concat ":"
                                       (mapconcat 'identity (split-string people ",+ *") ":")
                                       ":") "\n")))

(defvar org-journal-notes-directory "~/Notes/org/Journal")
(defun org-new-journal-entry-daily ()
  "Create a new journal entry and fill in header information."
  (interactive)
  (find-file (concat (file-name-as-directory org-meeting-notes-directory)
                                    (concat (get-current-date)
                                            ".org")))
  (insert (concat "#+Date: " (get-current-datetime) "\n"
                  "#+Author: Eric Jones\n")))

;; Windows screenshot function
;; shell command for Windows:
;;(setq os-shell-screenclip-command "snippingtool /clip")
;;(setq os-shell-write-screenclip (concat "powershell -command \"Add-Type -AssemblyName System.Windows.Forms;if ($([System.Windows.Forms.Clipboard]::ContainsImage())) {$image = [System.Windows.Forms.Clipboard]::GetImage();[System.Drawing.Bitmap]$image.Save('" filename "',[System.Drawing.Imaging.ImageFormat]::Png); Write-Output 'clipboard content saved as file'} else {Write-Output 'clipboard does not contain image data'}\""))
(defun os-sanitize-filename (string)
  "Take an input string and replace all reserved characters with underscores."
  (replace-regexp-in-string "[<>\:\*\\\|\/\"\?\*]" "_" string))

(defun os-screenclip-to-file (path)
  "Takes a screenclip and saves it to a file in an XFCE4 desktop."
  (interactive "FPath:")
  (shell-command (concat "xfce4-screenshooter --region --save \"" path "\"")))

(defun org-insert-screenclip ()
  "Take a screenclip into a named and time-stamped file in the same directory as the open buffer and insert a link to the file."
  (interactive)
  (let ((description (read-string "Clip description (blank for inline):")))
    (let ((defaultfilename (concat (os-sanitize-filename description) (get-current-datetime-hyphenated) ".png")))
      (let ((filename (read-file-name (concat "Filename [" defaultfilename "]:") nil defaultfilename)))
        (os-screenclip-to-file filename)
        (org-insert-link nil (concat "file:" filename) description)))))
