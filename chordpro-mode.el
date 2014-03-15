(require 'derived)

(defvar chordpro-font-lock-defaults
  '((("\\(\\[[^]]*\\]\\)" . font-lock-string-face)
     ("^\\(#.*\\)" . font-lock-comment-face)
     ("\\({subtitle[^}]*}\\)" . font-lock-type-face)
     ("\\({title[^}]*}\\)" . font-lock-keyword-face)
     ("\\({[^}]*}\\)" . font-lock-variable-name-face))))
       
(define-derived-mode chordpro-mode text-mode "Chordpro"
  "Major mode for editing Chordpro files.
Special commands:
\\{chordpro-mode-map}"
  (setq font-lock-defaults chordpro-font-lock-defaults)
  (auto-fill-mode -1))

(define-key chordpro-mode-map "\C-ci" 'chordpro-insert-chord)
(define-key chordpro-mode-map "\C-cw" 'chordpro-kill-current-chord)
(define-key chordpro-mode-map "\C-cz" 'chordpro-kill-next-chord)
(define-key chordpro-mode-map "\C-cc" 'chordpro-copy-current-chord)
(define-key chordpro-mode-map "\C-cx" 'chordpro-copy-next-chord)
(define-key chordpro-mode-map "\C-cm" 'chordpro-insert-comment)
(define-key chordpro-mode-map "\C-ch" 'chordpro-insert-chorus)
(define-key chordpro-mode-map "\C-ct" 'chordpro-insert-title)
(define-key chordpro-mode-map "\C-cs" 'chordpro-insert-subtitle)

(defun chordpro-insert-chord (chord)
  "Prompt for and insert chord at point, performing some normalization."
  (interactive "MChord:")
  (insert "[" (chordpro-normalize-chord chord) "]"))

(defun chordpro-normalize-chord (chord)
  "Trim whitespace, capitalize first letter of chord."
  (capitalize (replace-regexp-in-string "\\s " "" chord)))
  
(defvar chordpro-chord-regexp
  "\\[[^][]*\\]"
  "Regexp for matching a chord without regard for the point.")

(defun chordpro-kill-next-chord ()
  "Kill the next full chord after the point and move point there."
  (interactive)
  (let ((start (re-search-forward chordpro-chord-regexp nil t)))
    (if start
        (progn 
          (kill-region (match-beginning 0) (match-end 0))
          (goto-char (match-beginning 0))))))

(defun chordpro-copy-next-chord ()
  "Copy the next full chord after the point to the kill ring."
  (interactive)
  (save-excursion
    (let ((start (re-search-forward chordpro-chord-regexp nil t)))
      (if start
          (copy-region-as-kill (match-beginning 0) (match-end 0))))))

(defun chordpro-kill-current-chord ()
  "Kill the chord surrounding the point, if there is one."
  (interactive)
  (operate-on-current-chord 'kill-region))

(defun chordpro-copy-current-chord ()
  "Copy the chord surrounding the point, if there is one."
  (interactive)
  (operate-on-current-chord 'copy-region-as-kill))

(defun operate-on-current-chord (function)
  "Call a two argument function on the current chord, if it exists, with
the start and end of the chord."
  (let ((current-position (point-marker)))
    (save-excursion
      (let ((start-found (search-backward "[" nil t)))
        (if start-found
            (let* ((start (point-marker))
                   (end-found (search-forward "]" nil t)))
              (if end-found
                  (let ((end (point-marker)))
                    (if (and (<  start current-position)
                             (< current-position end))
                        (funcall function start end))))))))))

(defun chordpro-insert-single-directive (text)
  (insert "{" text ": }\n")
  (search-backward "}"))

(defun chordpro-insert-comment ()
  "Insert a chordpro comment."
  (interactive)
  (chordpro-insert-single-directive "comment"))

(defun chordpro-insert-title ()
  "Insert a chordpro title."
  (interactive)
  (chordpro-insert-single-directive "title"))

(defun chordpro-insert-subtitle ()
  "Insert a chordpro subtitle."
  (interactive)
  (chordpro-insert-single-directive "subtitle"))

(defun chordpro-insert-chorus ()
  "Insert a chordpro chorus."
  (interactive)
  (insert "{start-of-chorus}\n\n{end-of-chorus}\n")
  (search-backward "\n" nil nil 2))
  
(provide 'chordpro)

