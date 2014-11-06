(defun
  my-replace-regex (regexp to-string)
  (while (re-search-forward regexp nil t)
    (replace-match to-string nil nil)))

(defun
  my-replace-string (from to)
  (while (search-forward from nil t)
    (replace-match to nil t)))

(defun php-cleanup ()
  "Cleans a php file"
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (my-replace-string "
" "
")

    (goto-char (point-min))(my-replace-string "if(" "if (")
    (goto-char (point-min))(my-replace-string "while(" "while (")
    (goto-char (point-min))(my-replace-string "foreach(" "foreach (")
    (goto-char (point-min))(my-replace-string "switch(" "switch (")
    (goto-char (point-min))(my-replace-string "for(" "for (")
    (goto-char (point-min))(my-replace-string "array (" "array(")

    (goto-char (point-min))(my-replace-string "){" ") {")

    (if
	(string-match "yahoo.com$" (system-name))
	(progn
	  (goto-char (point-min))
	  (my-replace-regex "[ \t]function\\([^(]+\\)(\\([^)]*\\))[ \t\n]+{" " function\\1(\\2)\n{")
	  (goto-char (point-min))
	  (my-replace-regex "class\\([^{]+\\)[ \t\n]+{" "class\\1\n{")
	  (goto-char (point-min))(my-replace-regex "([ \t]+" "(")
	  )
      (progn
       (goto-char (point-min))(my-replace-regex ")[ \t]*{" ") {")
       (goto-char (point-min))(my-replace-regex "([ \t]*\n[ \t]+" "(\n")
       (goto-char (point-min))
       (my-replace-regex "[ \t]function\\([^(]+\\)(\\([^)]*\\))[ \t\n]+{" " function\\1(\\2) {")
       (goto-char (point-min))
       (my-replace-regex "class\\([^{]+\\)[ \t\n]+{" "class\\1 {")
       )
      )

    ; tighten up parentheses
    (goto-char (point-min))(my-replace-string "( " "(")
    (goto-char (point-min))(my-replace-string " )" ")")

    ; make br tags nicer.  ugh?
    (goto-char (point-min))(my-replace-string "<br>" "<br />")
    (goto-char (point-min))(my-replace-string "<BR>" "<br />")
    (goto-char (point-min))(my-replace-string "<br/>" "<br />")
    (goto-char (point-min))(my-replace-string "</ br>" "<br />")
    (goto-char (point-min))(my-replace-string "stdclass" "stdClass")

    ;(goto-char (point-min))(my-replace-string "\":" "\" :")
    ; assignment and concatenation with arrays
    ;(goto-char (point-min))(my-replace-string "]." "] .")
    ;(goto-char (point-min))(my-replace-string "]=" "] =")

    ; oh god what horror is this
    ;(goto-char (point-min))(my-replace-regex "\\([.,;]\\)\\([\\$'\"]\\)" "\\1 \\2")


    ; insert missing docblocks
    (goto-char (point-min))(my-replace-regex "\\([{};][ \t\n]*\\)\\(public\\|private\\|static\\|function\\|var\\|class\\|interface\\|abstract\\)" "\\1\n/**\n *\n */\n\\2")

    ; else on the same line as the closing brace of the if
    (goto-char (point-min))(my-replace-regex "}[ \t\n]*else" "} else")

    ; opening else brace on the same line as the else
    (goto-char (point-min))(my-replace-regex "else[ \t\n]*{" "else {")

    ; key => spacing in associative arrays
    (goto-char (point-min))(my-replace-regex "\\([^ \t]\\)=>" "\\1 =>")

    ; close parentheses at the end of a line with trailing whitespace
    (goto-char (point-min))(my-replace-regex ")[ \t]+\n" ")\n")

    ; => value spacing in associative arrays
    (goto-char (point-min))(my-replace-regex "=>\\([^ \t]\\)" "=> \\1")

    ; key => spacing in associative arrays
    (goto-char (point-min))(my-replace-regex "\\([^ \t]\\)[ \t][ \t]+=>" "\\1 =>")

    ; => value spacing in associative arrays
    (goto-char (point-min))(my-replace-regex "=>[ \t][ \t]+" "=> ")

    ; move commas to the end of lines
    ; unsafe inside of strings?
    ;(goto-char (point-min))(my-replace-regex "\n[ \t]+," ",\n")

    ; trim whitespace after semicolons at the end of lines
    (goto-char (point-min))(my-replace-regex ";[ \t]+\n" ";\n")

    ; delete whitespace from in front of commas
    ; unsafe inside of strings?
    ;(goto-char (point-min))(my-replace-regex "\W+," ",")

    ; convert // comments into /* comments */
    (goto-char (point-min))(my-replace-regex "\n\\([ \t]\\)*//+[ \t]*\\([^\n]+\\)" "\n\\1/* \\2 */")

    ; convert sequential comments into multiline comments
    (goto-char (point-min))(my-replace-regex "\\*\\/\n\\([ \t]\\)*\\/\\*" "\n*")

    ; Remove trailing ?> from files
    (goto-char (point-min))(my-replace-regex "\\?>[ \n\t]*\\'" "\n")


    (delete-trailing-whitespace)
    (indent-region (point-min) (point-max) nil)

    (untabify (point-min) (point-max))

  )
)
(provide 'php-cleanup)
