;;Datetime insert functions
(defun get-current-datetime ()
  "Gets the current local datetime with timezone as a string."
  (format-time-string "%F %T %z"))

(defun get-current-datetime-hyphenated ()
  "Gets the current local datetime as a hyphen-separated string."
  (format-time-string "%Y-%m-%d-%H-%M-%S"))

(defun get-current-time ()
  "Gets the current local time as a string."
  (format-time-string "%T"))

(defun get-current-date ()
  "Gets the current local date in ISO format as a string."
  (format-time-string "%F"))

(defun get-current-unixtime ()
  "Gets the unix timestamp in seconds-since-epoch as a string."
  (format-time-string "%s"))
  
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
