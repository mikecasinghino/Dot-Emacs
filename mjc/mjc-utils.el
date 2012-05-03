(defun swap-windows ()
  "If you have 2 windows, it swaps them. (Thanks, stevey)"
  (interactive)
  (cond ((not (= (count-windows) 2))
         (message "You need exactly 2 windows to do this"))
        (t
         (let* ((w1 (first (window-list)))
                (w2 (second (window-list)))
                (b1 (window-buffer w1))
                (b2 (window-buffer w2))
                (s1 (window-start w1))
                (s2 (window-start w2))
                (w1-selected (eq (selected-window) w1)))
           (set-window-buffer w1 b2)
           (set-window-buffer w2 b1)
           (set-window-start w1 s2)
           (set-window-start w2 s1)
           (if w1-selected
               (select-window w2)
             (select-window w1))))))

;; These next two helped during the transition from vim
(defun forward-next-word-under-point ()
  "Move forward to the next word under the point"
  (interactive)
  (let ((word (current-word)))
    (message "searching for: %s" word)
    (if (string= word "")
        (message "no word under point")
        (progn
          (goto-char (+ 1 (point)))
          (if (not (search-forward word nil 'beg))
              (progn
                (goto-char (point-min))
                (search-forward word)))
          (search-backward word)))))

(defun backward-next-word-under-point ()
  "Move backward to the next word uner the point"
  (interactive)
  (let ((word (current-word)))
    (if (string= word "")
        (message "no word under point")
      (progn
        (message "searching for: %s" word)
        (if (not (search-backward word nil 'beg))
            (progn
              (goto-char (point-max))
              (search-backward word)))
        (search-backward word)))))

(defun week-ending ()
  "Insert a week-ending message in the daily.log"
  (interactive)
  (let* ((parts (decode-time))
         (adj (seconds-to-time (* 86400 (- 5 (elt parts 6)))))
         (time (time-add (current-time) adj)))
    (insert (concat "#week ending "
                    (format-time-string "%Y-%m-%d %a" time)))))

(defun date ()
  "Insert a date stamp"
  (interactive)
  (let ((dstr (format-time-string "%Y-%m-%d %a")))
    (insert dstr)))

(defun frame-widen ()
  "Widens the selected frame by 80 columns"
  (interactive)
  (let ((w (frame-width))
        (h (frame-height)))
    (set-frame-size (selected-frame) (+ 80 w) h)))

(defun frame-narrow ()
  "Narrows the selected frame by 80 columns"
  (interactive)
  (let ((w (frame-width))
        (h (frame-height)))
    (set-frame-size (selected-frame) (- w 80) h)))

(defun frame-add-new-vert ()
  "Adds a new vertical window to the frame"
  (interactive)
  (frame-widen)
  (let ((wnew (split-window (first (reverse (window-list))) nil t)))
    (set-window-buffer wnew (get-buffer-create "*scratch*"))))

(defun frame-delete-last-window ()
  "Deletes the last window in the window list"
  (interactive)
  (when (> (length (window-list)) 1)
    (delete-window (first (reverse (window-list))))
    (frame-narrow)))

(defun vsplit-to-horiz ()
  "If the current window is split vertically, make it split horizontally"
  (interactive)
  (when (> (length (window-list)) 2)
    (message "Can't split with more than 2 windows")
    (return))
  (if (eql (length (window-list)) 1)
      (frame-add-new-vert)
    (let ((w1 (first (window-list)))
          (w2 (second (window-list))))
      (let ((b2 (window-buffer w2)))
        (delete-window w2)
        (frame-widen)
        (let ((wnew (split-window w1 nil t)))
          (set-window-buffer wnew b2))))))

(defun fill-line-dashes ()
  "Fills from point to column 60 with dashes"
  (interactive)
  (save-excursion
    (let* ((pcur (point))
           (pbeg (line-beginning-position))
           (cdashes (- 60 (- pcur pbeg))))
      (insert (make-string cdashes ?-)))))

(defun dos2unix ()
  "Uses set-buffer-file-coding-system to unix"
  (interactive)
  (set-buffer-file-coding-system 'unix))

(defun unix2dos ()
  "Uses set-buffer-file-coding-system to dos"
  (interactive)
  (set-buffer-file-coding-system 'dos))

(defun underline (&optional uchar)
  "Underline the current line of text with the character given"
  (interactive)
  (when (null uchar)
    (setf uchar ?-))
  (save-excursion
    (let ((line-len (length
                     (buffer-substring (line-beginning-position)
                                       (line-end-position))))
          newline)
      ;; If there's no terminating \n after this line, add it
      (when (eql (line-end-position) (point-max))
        (setq newline t))
      (forward-line)
      (when newline (insert "\n"))
      ;; Look for a previous underline and remove it
      (when (re-search-forward (concat "^" (string uchar) "+$")
                               (line-end-position) t)
        (goto-char (line-beginning-position))
        (kill-whole-line))
      (insert (make-string line-len uchar))
      (insert "\n"))))

(defun date-to-dow (date)
  "Parses a date in the form 2001-01-01 and returns its day of the week"
  (string-match "\\([0-9]+\\)-\\([0-9]+\\)-\\([0-9]+\\)" date)
  (let* ((date-list
         (mapcar #'read (list (match-string 1 date)
                              (match-string 2 date)
                              (match-string 3 date))))
         (time-list (append '(0 0 0)
                            (reverse date-list))))
    (format-time-string "%a" (apply #' encode-time time-list))))

(defun insert-day-of-week ()
  "Looks at the current word and inserts the day of the week after
it if it looks like a date in the form 2008-01-31"
  (interactive)
  (let ((dow-str (date-to-dow (current-word))))
    (when dow-str
      (backward-word)
      (forward-word)
      (insert " " dow-str))))

(defun toggle-show-trailing-whitespace ()
  (interactive)
  (setq show-trailing-whitespace (not show-trailing-whitespace))
  (if show-trailing-whitespace
      (message "trailing whitespace enabled")
      (message "trailing whitespace disabled")))

(defun memberize (str)
  (concat "m" (upcase (substring str 0 1)) (substring str 1)))

(defun memberize-curword ()
  (interactive)
  (let ((cw (current-word)))
    (forward-word)
    (backward-word)
    (insert (memberize cw))
    (kill-word 1)))

(defun find-ch (other-win-p dir1 dir2 ext)
  (let ((fname (buffer-file-name))
        (open-fn (if other-win-p 'find-file-other-window 'find-file)))
    (when fname
      (let* ((basename (file-name-nondirectory fname))
             (fdir (file-name-directory fname))
             (updir (file-name-directory (directory-file-name fdir)))
             (pdir (file-name-nondirectory (directory-file-name fdir)))
             (hfile (concat updir dir1 (file-name-sans-extension basename)
                            ext)))
        (when (string-equal pdir dir2)
          (funcall open-fn hfile))))))

(defun find-ch-here (other-win-p look-for-c-file-p)
    (let* ((base (file-name-sans-extension (buffer-file-name)))
           (open-fn (if other-win-p 'find-file-other-window 'find-file))
           (exts (if look-for-c-file-p '(".c" ".cpp") '(".h" ".hpp")))
           (hfiles (remove-if-not #'file-exists-p
                           (mapcar #'(lambda (x) (concat base x)) exts)))
           (the-file (car hfiles)))
      (when the-file
        (funcall open-fn the-file))))

(defun find-h (arg)
  "Look in some relative paths for the header to the file open in
the active buffer"
  (interactive "P")
  (unless (find-ch arg "inc/" "src" ".h")
    (find-ch-here arg nil)))

(defun find-c (arg)
  "Look in some relative paths for the c file for the header open
in the active buffer"
  (interactive "P")
  (unless (find-ch arg "src/" "inc" ".cpp")
    (find-ch-here arg t)))

(defun camelify ()
  (interactive)
  (flet ((camelify-match
          (data cnt)
          (upcase (substring (match-string 0) 1 2))))
    (perform-replace "_[a-z]"
                     (cons 'camelify-match nil)
                     t t nil)))

(defun term-here ()
  (interactive)
  (if (not (null (getenv "DISPLAY")))
    (let* ((cmd (format "cd \"%s\" && exec /bin/zsh -i" default-directory))
      (message "DISPLAY variable is set to \'%S\'" (getenv "DISPLAY"))
           (args (list "-sl" "1500"
                       "-fn" "Consolas-14"
                       "-geometry" "100x35"
                       "-bg" "black"
                       "-fg" "green"
                       "+sb" "-e"
                       "zsh" "--login" "-c" cmd)))
      (apply 'start-process "rxvt" nil "C:/bin/rxvt.exe" args))))

(defun lookup ()
  "Lookup the current word at dictionary.reference.com"
  (interactive)
  (browse-url (concat "http://www.merriam-webster.com/dictionary/"
                      (current-word))))

(defun show-path (&optional path)
  "Open a new buffer with the PATH environment variable or the
 string argument displayed in it"
  (interactive)
  (when (null path)
    (setq path (getenv "PATH")))
  (with-output-to-temp-buffer "show-path"
    (let ((parts (parse-colon-path path)))
      (dolist (p parts)
        (princ p)
        (terpri)))))

(defun is-windows-p ()
  (eql system-type 'windows-nt))

(defun in-path-p (dirname)
  "Checks whether 'dirname' is in the PATH env var"
  (let ((pathvar (getenv "PATH")))
    (when (is-windows-p)
      (setq pathvar (upcase pathvar))
      (setq dirname (upcase dirname)))
    (let ((paths (split-string pathvar path-separator)))
      (member dirname paths))))

(defun append-path (dirname)
  "Append dirname onto the PATH env var unless it's already in it"
  (interactive)
  (when (not (in-path-p dirname))
    (setenv "PATH" (concat (getenv "PATH") path-separator dirname))))

(defun prepend-path (dirname)
  "Prepend dirname onto the PATH env var unless it's already in it"
  (interactive)
  (when (not (in-path-p dirname))
    (setenv "PATH" (concat path-separator dirname (getenv "PATH")))))

(defun append-cygwin-path ()
  "Append C:\\bin to the path environment variable"
  (interactive)
  (append-path "C:\\bin"))

(defun write-string-to-file (string fname)
  (with-temp-buffer
    (insert string)
    (when (file-writable-p fname)
      (write-region (point-min) (point-max) fname))))

(defun insert-line-above ()
  "Inserts a line above the current line and moves point there"
  (interactive)
  (goto-char (line-beginning-position))
  (insert "\n")
  (previous-line))

(defun buffer-to-ascii ()
  "Translate fancy punctuation characters into standard ascii"
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (replace-regexp (string 8217) "'")
    (goto-char (point-min))
    (replace-regexp (string 8201) " ")
    (goto-char (point-min))
    (replace-regexp (string 8220) "\"")
    (goto-char (point-min))
    (replace-regexp (string 8221) "\"")
    (goto-char (point-min))
    (replace-regexp (string 8212) "-")))

(defun fill-all-paragraphs ()
  "Fill all paragraphs in buffer"
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (while (< (point) (point-max))
      (fill-paragraph)
      (forward-paragraph))))

(defun gtypify-buffer ()
  "Turn the text of a buffer into a gtypist speed drill"
  (interactive)
  (fill-all-paragraphs)
  (buffer-to-ascii)
  (save-excursion
    (goto-char (point-max))
    (delete-blank-lines)
    (goto-char (point-min))
    (insert "B:")
    (date)
    (insert "\n")
    (insert "S:")
    (replace-regexp "^" " :")))

(defun dbl-click ()
  "Run 'start' on the current file"
  (interactive)
  (start-process "start" "*start*" "c:/bin/cygstart" (buffer-file-name)))

(defun xp ()
  "Open an explorer window for the current file"
  (interactive)
  (let ((arg (concat "/select,"
                     (replace-regexp-in-string "/" "\\\\" (buffer-file-name)))))
    ;(message arg)))
    (start-process "xp" "*xp*" "explorer" arg)))

(defun cb-directory ()
  "Copy the directory of the current buffer's file into the clipboard"
  (interactive)
  (kill-new (file-name-directory (buffer-file-name))))

(defun cb-filename ()
  "Copy the name of the current buffer's file into the clipboard"
  (interactive)
  (kill-new (buffer-file-name)))

(setq *svn-prog-location* "C:\\PROGRA~2\\COLLAB~1\\SUBVER~1\\svn.exe")
(defun svn-diff (arg)
  "Run svn diff on the current buffer (or directory) using winmerge"
  (interactive "P")
  (let ((target (if arg
                    (file-name-directory (buffer-file-name))
                  (buffer-file-name))))
    (start-process "svn-diff" "*svn*"
                   *svn-prog-location* "diff"
                   "--diff-cmd=svn-diffmerge.bat"
                   target)))

(defun svn-oldest ()
  (interactive)
  (let ((fname (cb-filename)))
    (with-temp-buffer
      (shell-command (concat *svn-prog-location* " log " fname) t)
      (goto-char (point-max))
      (re-search-backward "^r[[:digit:]]+")
      (re-search-forward "| 20[[:digit:][:digit:]]")
      (message (current-word)))))

(provide 'mjc-utils)
