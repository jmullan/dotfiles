(defun
  my-replace-regex (regexp to-string)
  (while (re-search-forward regexp nil t)
    (replace-match to-string nil nil)))

(defun
  my-replace-string (from to)
  (while (search-forward from nil t)
    (replace-match to nil t)))

(defun css-cleanup ()
  "Cleans a php file"
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (my-replace-string "
" "
")
    (goto-char (point-min))(my-replace-string "{" " {\n")
    (goto-char (point-min))(my-replace-string "}" "\n}\n")
    (goto-char (point-min))(my-replace-string "\n\n}" "\n}\n")
    (goto-char (point-min))(my-replace-string ":" ": ")
    (goto-char (point-min))(my-replace-string "," ", ")
    (goto-char (point-min))(my-replace-string "important!" "!important")
    (goto-char (point-min))(my-replace-string "/*" "/* ")
    (goto-char (point-min))(my-replace-string "*/" " */")
    (goto-char (point-min))(my-replace-regex "[ \t\n][ \t\n]+{" " {")
    (goto-char (point-min))(my-replace-regex "{[ \t\n]+" "{\n")
    (goto-char (point-min))(my-replace-regex ":[ \t\n]+" ": ")
    (goto-char (point-min))(my-replace-regex ": \\(active\\|focus\\|hover\\|link\\|visited\\|first_child\\|lang\\)" ":\\1")
    (goto-char (point-min))(my-replace-regex ",[ \t]+" ", ")
    (goto-char (point-min))(my-replace-regex "/\\*[ \t\n]+" "/* ")
    (goto-char (point-min))(my-replace-regex "[ \t\n]+\\*/" " */")

    (delete-trailing-whitespace)
    (indent-region (point-min) (point-max) nil)
    (untabify (point-min) (point-max))
    )
  )
(provide 'css-cleanup)
