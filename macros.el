;;Datetime insert functions
(defun insert-current-datetime ()
  "Inserts the current local datetime with timezone."
  (interactive)
  (insert (shell-command-to-string "echo -n $(date \"+%F %T %Z\")")))

(defun insert-current-time ()
  "Inserts the current local time."
  (interactive)
  (insert (shell-command-to-string "echo -n $(date \"+%T\")")))

(defun insert-current-date ()
  "Inserts the current local date in ISO format."
  (interactive)
  (insert (shell-command-to-string "echo -n $(date \"+%F\")")))

(defun insert-current-unixtime ()
  "Inserts the unix timestamp in seconds-since-epoch."
  (interactive)
  (insert (shell-command-to-string "echo -n $(date \"+%s\")")))

(global-set-key (kbd "C-; d") 'insert-current-date)
(global-set-key (kbd "C-; t") 'insert-current-time)
(global-set-key (kbd "C-; ;") 'insert-current-datetime)
(global-set-key (kbd "C-; :") 'insert-current-unixtime)
