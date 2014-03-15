(require 'derived)

(defvar chordpro-font-lock-keywords
'(("\\(\\[[^]]*\\]\\)" . font-lock-string-face)
  ("^\\(#.*\\)" . font-lock-comment-face)
  ("\\({subtitle[^}]*}\\)" . font-lock-type-face)
  ("\\({title[^}]*}\\)" . font-lock-keyword-face)
  ("\\({[^}]*}\\)" . font-lock-variable-name-face)))
       
(define-derived-mode chordpro-mode text-mode "Chordpro"
  "Major mode for editing Chordpro files.
Special commands:
\\{chordpro-mode-map}"
  (chordpro-add-font-lock))

(defun chordpro-add-font-lock ()
  (font-lock-add-keywords 
   'chordpro-mode
   chordpro-font-lock-keywords))                       

(define-key chordpro-mode-map "\C-ci" 'chordpro-insert-chord)
(define-key chordpro-mode-map "\C-cw" 'chordpro-kill-current-chord)
(define-key chordpro-mode-map "\C-cz" 'chordpro-kill-next-chord)

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

(defun chordpro-kill-current-chord ()
  "Kill the chord surrounding the point."
  (interactive)
  (let ((current-position (point-marker)))
    (save-excursion
      (let ((start-found (search-backward "[" nil t)))
        (if start-found
            (let* ((start (point-marker))
                   (end-found (search-forward "]" nil t)))
              (if end-found
                  (let ((end (point-marker)))
                    (if (and (< (marker-position start) (marker-position current-position))
                             (< (marker-position current-position) (marker-position end)))
                        (kill-region start end))))))))))
    
  
(provide 'chordpro)

