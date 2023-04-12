(require 'derived)
(require 'dropdown-list nil t)

(defvar chordpro-font-lock-defaults
  '((("\\(\\[[^]]*\\]\\)" . font-lock-string-face)
     ("^\\(#.*\\)" . font-lock-comment-face)
     ("\\({subtitle[^}]*}\\)" . font-lock-type-face)
     ("\\({title[^}]*}\\)" . font-lock-keyword-face)
     ("\\({[^}]*}\\)" . font-lock-variable-name-face))))

(defvar chordpro-file-encoding 'latin-1)
 
;;;###autoload
(define-derived-mode chordpro-mode text-mode "Chordpro"
  "Major mode for editing Chordpro files.
Special commands:
\\{chordpro-mode-map}"
  (setq font-lock-defaults chordpro-font-lock-defaults)
  (setq buffer-file-coding-system chordpro-file-encoding)
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
(define-key chordpro-mode-map "\C-cl" 'chordpro-choose-insert-chord)
(define-key chordpro-mode-map "\C-cr" 'chordpro-choose-replace-current-chord)
(define-key chordpro-mode-map "\C-\M-n" 'chordpro-current-chord-forward)
(define-key chordpro-mode-map "\C-\M-p" 'chordpro-current-chord-backward)
(define-key chordpro-mode-map [C-down-mouse-1] 'mouse-set-point)
(define-key chordpro-mode-map [C-mouse-1] 'chordpro-kill-current-chord)
(define-key chordpro-mode-map [C-down-mouse-2] 'mouse-set-point)
(define-key chordpro-mode-map [C-mouse-2] 'chordpro-mouse-choose-insert-chord)
(define-key chordpro-mode-map [C-down-mouse-3] 'mouse-set-point)
(define-key chordpro-mode-map [C-mouse-3] 'chordpro-kill-next-chord)
(define-key chordpro-mode-map [S-down-mouse-1] 'mouse-set-point)
(define-key chordpro-mode-map [S-mouse-1] 'chordpro-copy-current-chord)
(define-key chordpro-mode-map [S-down-mouse-2] 'mouse-set-point)
(define-key chordpro-mode-map [S-mouse-2] 'chordpro-mouse-insert-chord)
(define-key chordpro-mode-map [S-down-mouse-3] 'mouse-set-point)
(define-key chordpro-mode-map [S-mouse-3] 'chordpro-copy-next-chord)

(defun chordpro-insert-chord (chord)
  "Prompt for and insert chord at point, performing some normalization."
  (interactive "*MChord:")
  (insert "[" (chordpro-normalize-chord chord) "]"))

(defun chordpro-mouse-insert-chord (event chord)
  "Prompt for and insert chord at point, performing some normalization."
  (interactive "@e\nMChord:")
  (insert "[" (chordpro-normalize-chord chord) "]"))

(defun chordpro-choose-insert-chord ()
  "Insert a chord chosen from a dropdown menu that contains all chords
already in the document."
  (interactive)
  (when (featurep 'dropdown-list)
    (let* ((choices (chordpro-buffer-chord-list))
           (selection (dropdown-list choices)))
      (when selection
        (chordpro-insert-chord (nth selection choices))))))

(defun chordpro-mouse-choose-insert-chord (event)
  "Insert a chord chosen from a dropdown menu that contains all chords
already in the document."
  (interactive "@e")
    (when (featurep 'dropdown-list)
    (let* ((choices (chordpro-buffer-chord-list))
           (selection (dropdown-list choices)))
      (when selection
        (chordpro-insert-chord (nth selection choices))))))

;;;This could be done more efficiently, but for most usages
;;;it shouldn't be a problem to just scan the whole document each time
(defun chordpro-buffer-chord-list ()
  "Return a list of the chords currently used in the document."
  (interactive)
  (let ((chords nil))
    (save-excursion
      (save-match-data
        (goto-char (point-min))
        (while (re-search-forward chordpro-chord-regexp nil t)
          (add-to-list 'chords (match-string 1)))))
    (sort chords 'string<)))

(defun chordpro-choose-replace-current-chord ()
  "Replace the current chord with one chosen from a dropdown list"
  (interactive)
  (when (featurep 'dropdown-list)
    (let* ((choices (chordpro-buffer-chord-list))
           (selection (dropdown-list choices)))
      (when selection
        (chordpro-delete-current-chord)
        (chordpro-insert-chord (nth selection choices))))))

(defun chordpro-normalize-chord (chord)
  "Trim whitespace, capitalize first letter of chord."
  (capitalize (replace-regexp-in-string "\\s " "" chord)))
  
(defvar chordpro-chord-regexp
  "\\[\\([^][]*\\)\\]"
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
  (chordpro-operate-on-current-chord 'kill-region))

(defun chordpro-delete-current-chord ()
  "Delete the chord surrounding the point, if there is one."
  (interactive)
  (chordpro-operate-on-current-chord 'delete-region))

(defun chordpro-copy-current-chord ()
  "Copy the chord surrounding the point, if there is one."
  (interactive)
  (chordpro-operate-on-current-chord 'copy-region-as-kill))

(defun chordpro-operate-on-current-chord (function)
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

(defun chordpro-current-chord-forward (n)
  "Move the current chord forward n characters."
  (interactive "*p")
  (let ((current-position (point-marker))
        (chord-offset (chordpro-position-in-current-chord)))
    (set-marker-insertion-type current-position t)
    (chordpro-operate-on-current-chord
     (lambda (start end)
       (kill-region start end)
       (forward-char n)
       (yank)))
    ;;I have to assume there's a better way to do this, but this works
    ;;Get back in the chord and then move to the offset
    (if (> n 0)
        (goto-char (+ current-position n 1))
      (goto-char (- current-position (+ n 4))))
    (chordpro-move-to-position-in-current-chord chord-offset)))
     
(defun chordpro-current-chord-backward (n)
  "Move the current chord backward n characters."
  (interactive "*p")
  (chordpro-current-chord-forward (- n)))

(defun chordpro-move-to-position-in-current-chord (n)
  "Move to the nth character in the current chord."
  (search-backward "[")
  (forward-char n))

(defun chordpro-position-in-current-chord ()
  "Determine the offset inside the current chord."
  (interactive)
  (let ((current-position (point)))
    (save-excursion
      (search-backward "[")
      (- current-position (point)))))

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

