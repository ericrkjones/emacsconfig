(require 'org)
(setq org-support-shift-select t)
(setq org-startup-truncate t)

;; Silly duplicate key mapping entry.
;; Fix this by putting all custom keymaps into a minor mode and enabling
;; that minor mode at all times with priority over other modes. 
(define-key org-mode-map (kbd "<C-tab>") 'tabbar-forward-tab)
(define-key org-mode-map (kbd "<C-iso-leftab>") 'tabbar-backward-tab)

;; Define functions for export from org and email.
;; Evolution CLI does not seem to support putting a complex body in an email in any way,
;; Including opening a file from disk.  So the stupid solution is to just attach the
;; file to the email instead of making the email from the file.
(defvar org-email-cleanup t)
(defun org-email-attachment-html (subject)
  "Compile the org file to an html file in its directory, then open the default email editor to send that file as an attachment."
  (interactive "MSubject:")
  (shell-command (concat "exo-open "
                         (shell-quote-argument (concat "mailto:?subject="
                                                       (url-encode-url subject)
                                                       "&attach="
                                                       (file-name-nondirectory (org-html-export-to-html)))))))

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
