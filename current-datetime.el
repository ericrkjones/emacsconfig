;;Datetime insert functions
(defun get-current-datetime ()
  "Gets the current local datetime with timezone as a string."
  (shell-command-to-string "echo -n $(date \"+%F %T %Z\")"))

(defun get-current-time ()
  "Gets the current local time as a string."
  (shell-command-to-string "echo -n $(date \"+%T\")"))

(defun get-current-date ()
  "Gets the current local date in ISO format as a string."
  (shell-command-to-string "echo -n $(date \"+%F\")"))

(defun get-current-unixtime ()
  "Gets the unix timestamp in seconds-since-epoch as a string."
  (shell-command-to-string "echo -n $(date \"+%s\")"))
  
(defun insert-current-datetime ()
  "Inserts the current local datetime with timezone."
  (interactive)
  (insert (get-current-datetime)))

(defun insert-current-time ()
  "Inserts the current local time."
  (interactive)
  (insert (get-current-time)))

(defun insert-current-date ()
  "Inserts the current local date in ISO format."
  (interactive)
  (insert (get-current-date)))

(defun insert-current-unixtime ()
  "Inserts the unix timestamp in seconds-since-epoch."
  (interactive)
  (insert (get-current-unixtime)))

(global-set-key (kbd "C-; d") 'insert-current-date)
(global-set-key (kbd "C-; t") 'insert-current-time)
(global-set-key (kbd "C-; ;") 'insert-current-datetime)
(global-set-key (kbd "C-; :") 'insert-current-unixtime)
