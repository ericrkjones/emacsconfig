;; Hyperbind single
;; The hyper binding is not used generally, so the point here is to make
;; a keybinding for hyper inherit all the keybindings for another key.
;; For example, I use "H-{ijkl}" as a replacement for UDLR, and I want
;; "<C-H-M-S-i>" to do the same thing as "<C-M-S-up>".

(defun global-set-hyperbind (input output)
  (dolist (modifier '(""
                      "C-" "M-" "s-" "S-"
                      "C-M-" "C-s-" "C-S-" "M-s-" "M-S-" "s-S-"
                      "C-M-s-" "C-M-S-" "C-s-S-" "M-s-S-"
                      "C-M-s-S-") output)
    (global-set-key (kbd (concat "H-" modifier input)) (kbd (concat modifier output)))))

;; Because Emacs reads key characters and not keypresses, there is an issue with the convention here.
;; this command must be called for both the lower and upper cases of the key separately.
;; It is because S-b != B, and I am not entirely sure it is handled correctly here.
;; This is only true for alphabetical characters and is super annoying.

(global-set-hyperbind "i" "<up>")
(global-set-key (kbd "H-I") (kbd "S-<up>"))
(global-set-hyperbind "k" "<down>")
(global-set-key (kbd "H-K") (kbd "S-<down>"))
(global-set-hyperbind "j" "<left>")
(global-set-key (kbd "H-J") (kbd "S-<left>"))
(global-set-hyperbind "l" "<right>")
(global-set-key (kbd "H-L") (kbd "S-<right>"))
(global-set-hyperbind "u" "<home>")
(global-set-key (kbd "H-U") (kbd "S-<home>"))
(global-set-hyperbind "o" "<end>")
(global-set-key (kbd "H-O") (kbd "S-<end>"))
