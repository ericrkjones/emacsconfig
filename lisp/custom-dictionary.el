(require 'dictionary)
 (defun my-dictionary-search ()
   (interactive)
   (let ((word (current-word))
         (enable-recursive-minibuffers t)
         (val))
     (setq val (read-from-minibuffer
                (concat "Word"
                        (when word
                          (concat " (" word ")"))
                        ": ")))
     (dictionary-new-search
      (cons (cond
              ((and (equal val "") word)
               word)
              ((> (length val) 0)
               val)
              (t
               (error "No word to lookup")))
            dictionary-default-dictionary))))
