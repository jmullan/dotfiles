(defun
  my-replace-string (from to)
  (while (search-forward from nil t)
    (replace-match to nil t)))

(defun js-cleanup ()
  "Cleans a js file"
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
    (goto-char (point-min))(my-replace-string "function(" "function (")
    (goto-char (point-min))(my-replace-string "array (" "array(")
    (goto-char (point-min))(my-replace-string "for (i=0;i<" "for (i = 0; i < ")
    (goto-char (point-min))(my-replace-string "for (var i=0;i<" "for (var i = 0; i < ")
    (goto-char (point-min))(my-replace-string ";i++" "; i++")
    (goto-char (point-min))(my-replace-string "){" ") {")
    (delete-trailing-whitespace)
    (indent-region (point-min) (point-max) nil)
    (untabify (point-min) (point-max))
    )
)
(provide 'js-cleanup)
