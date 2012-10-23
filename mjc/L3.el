(defun inc-annotate ()
  "Run svn info to find the current svn directory and browse to
the annotated version of the file from subversion"
  (interactive)
  (dolist (line (split-string (shell-command-to-string
                               "C:\\cygwin\\bin\\svn info") "\n"))
    (let ((idx (string-match "URL: " line)))
      (when (and idx (= idx 0))
        (let ((url (concat (replace-regexp-in-string 
                            "/svn/" "/viewvc/"
                            (substring line 4)) "/"
                            (file-name-nondirectory (buffer-file-name))
                            "?annotate=HEAD")))
          (browse-url url))))))

(provide 'L3)
