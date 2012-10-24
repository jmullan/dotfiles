(defun php-cleanup ()
  "Cleans a php file"
  (interactive)
  (save-excursion
    (beginning-of-buffer)
    (replace-string "
" "
")

    (beginning-of-buffer)(replace-string "if(" "if (")
    (beginning-of-buffer)(replace-string "while(" "while (")
    (beginning-of-buffer)(replace-string "foreach(" "foreach (")
    (beginning-of-buffer)(replace-string "switch(" "switch (")
    (beginning-of-buffer)(replace-string "for(" "for (")
    (beginning-of-buffer)(replace-string "array (" "array(")

    (beginning-of-buffer)(replace-string "){" ") {")

    (if
	(string-match "yahoo.com$" (system-name))
	(progn
	  (beginning-of-buffer)
	  (replace-regexp "[ \t]function\\([^(]+\\)(\\([^)]*\\))[ \t\n]+{" " function\\1(\\2)\n{")
	  (beginning-of-buffer)
	  (replace-regexp "class\\([^{]+\\)[ \t\n]+{" "class\\1\n{")
	  (beginning-of-buffer)(replace-regexp "([ \t]+" "(")
	  )
      (progn
       (beginning-of-buffer)(replace-regexp ")[ \t]*{" ") {")
       (beginning-of-buffer)(replace-regexp "([ \t]*\n[ \t]+" "(\n")
       (beginning-of-buffer)
       (replace-regexp "[ \t]function\\([^(]+\\)(\\([^)]*\\))[ \t\n]+{" " function\\1(\\2) {")
       (beginning-of-buffer)
       (replace-regexp "class\\([^{]+\\)[ \t\n]+{" "class\\1 {")
       )
      )

    ; tighten up parentheses
    (beginning-of-buffer)(replace-string "( " "(")
    (beginning-of-buffer)(replace-string " )" ")")

    ; make br tags nicer.  ugh?
    (beginning-of-buffer)(replace-string "<br>" "<br />")
    (beginning-of-buffer)(replace-string "<BR>" "<br />")
    (beginning-of-buffer)(replace-string "<br/>" "<br />")
    (beginning-of-buffer)(replace-string "</ br>" "<br />")
    (beginning-of-buffer)(replace-string "stdclass" "stdClass")

    ;(beginning-of-buffer)(replace-string "\":" "\" :")
    ; assignment and concatenation with arrays
    ;(beginning-of-buffer)(replace-string "]." "] .")
    ;(beginning-of-buffer)(replace-string "]=" "] =")

    ; oh god what horror is this
    ;(beginning-of-buffer)(replace-regexp "\\([.,;]\\)\\([\\$'\"]\\)" "\\1 \\2")


    ; insert missing docblocks
    (beginning-of-buffer)(replace-regexp "\\([{};][ \t\n]*\\)\\(public\\|private\\|static\\|function\\|var\\|class\\|interface\\|abstract\\)" "\\1\n/**\n *\n */\n\\2")

    ; else on the same line as the closing brace of the if
    (beginning-of-buffer)(replace-regexp "}[ \t\n]*else" "} else")

    ; opening else brace on the same line as the else
    (beginning-of-buffer)(replace-regexp "else[ \t\n]*{" "else {")

    ; key => spacing in associative arrays
    (beginning-of-buffer)(replace-regexp "\\([^ \t]\\)=>" "\\1 =>")

    ; close parentheses at the end of a line with trailing whitespace
    (beginning-of-buffer)(replace-regexp ")[ \t]+\n" ")\n")

    ; => value spacing in associative arrays
    (beginning-of-buffer)(replace-regexp "=>\\([^ \t]\\)" "=> \\1")

    ; key => spacing in associative arrays
    (beginning-of-buffer)(replace-regexp "\\([^ \t]\\)[ \t][ \t]+=>" "\\1 =>")

    ; => value spacing in associative arrays
    (beginning-of-buffer)(replace-regexp "=>[ \t][ \t]+" "=> ")

    ; move commas to the end of lines
    ; unsafe inside of strings?
    ;(beginning-of-buffer)(replace-regexp "\n[ \t]+," ",\n")

    ; trim whitespace after semicolons at the end of lines
    (beginning-of-buffer)(replace-regexp ";[ \t]+\n" ";\n")

    ; delete whitespace from in front of commas
    ; unsafe inside of strings?
    ;(beginning-of-buffer)(replace-regexp "\W+," ",")

    ; convert // comments into /* comments */
    (beginning-of-buffer)(replace-regexp "\n\\([ \t]\\)*//+[ \t]*\\([^\n]+\\)" "\n\\1/* \\2 */")

    ; convert sequential comments into multiline comments
    (beginning-of-buffer)(replace-regexp "\\*\\/\n\\([ \t]\\)*\\/\\*" "\n*")

    ; Remove trailing ?> from files
    (beginning-of-buffer)(replace-regexp "\\?>[ \n\t]*\\'" "\n")


    (delete-trailing-whitespace)
    (indent-region (point-min) (point-max) nil)

    (untabify (point-min) (point-max))

  )
)
(provide 'php-cleanup)