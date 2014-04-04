(defun css-cleanup ()
  "Cleans a php file"
  (interactive)
  (save-excursion
    (beginning-of-buffer)
    (replace-string "
" "
")
    (beginning-of-buffer)(replace-string "{" " {\n")
    (beginning-of-buffer)(replace-string "}" "\n}\n")
    (beginning-of-buffer)(replace-string "\n\n}" "\n}\n")
    (beginning-of-buffer)(replace-string ":" ": ")
    (beginning-of-buffer)(replace-string "," ", ")
    (beginning-of-buffer)(replace-string "important!" "!important")
    (beginning-of-buffer)(replace-string "/*" "/* ")
    (beginning-of-buffer)(replace-string "*/" " */")
    (beginning-of-buffer)(replace-regexp "[ \t\n][ \t\n]+{" " {")
    (beginning-of-buffer)(replace-regexp "{[ \t\n]+" "{\n")
    (beginning-of-buffer)(replace-regexp ":[ \t\n]+" ": ")
    (beginning-of-buffer)(replace-regexp ": \\(active\\|focus\\|hover\\|link\\|visited\\|first_child\\|lang\\)" ":\\1")
    (beginning-of-buffer)(replace-regexp ",[ \t]+" ", ")
    (beginning-of-buffer)(replace-regexp "/\\*[ \t\n]+" "/* ")
    (beginning-of-buffer)(replace-regexp "[ \t\n]+\\*/" " */")

    (delete-trailing-whitespace)
    (indent-region (point-min) (point-max) nil)
    (untabify (point-min) (point-max))
    )
  )
(provide 'css-cleanup)
