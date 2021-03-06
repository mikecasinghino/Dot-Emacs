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

;; This breaks yank, somehow
;(defun remove-text-properties ()
;  "Removes text properties from the current region"
;  (interactive)
;  ;; See also yank-excluded-properties and yank-handled-properties
;  (set-text-properties (point) (mark) nil))

(defun get-vert-wins ()
  "Return a list of windows in sorted by left edge"
  (sort (window-list)
        #'(lambda (w1 w2) (< (window-left-column w1) (window-left-column w2)))))

(defun swap-buffers (w1 w2)
  "Swap the buffers in windows w1 and w2"
  (let ((b1 (window-buffer w1))
        (b2 (window-buffer w2)))
    (set-window-buffer w1 b2)
    (set-window-buffer w2 b1)))

(defun swap-to-side (reverse?)
  "Swap the current window with the window to its right"
  (let ((wins (get-vert-wins)))
    (when reverse?
      (setf wins (reverse wins)))
    (while (not (and wins (eql (selected-window) (first wins))))
      (setf wins (cdr wins)))
    (when (and wins (cdr wins))
      (swap-buffers (first wins) (second wins)))))

(defun swap-right ()
  "Swap the current window with the window to its right"
  (interactive)
  (swap-to-side nil))

(defun swap-left ()
  "Swap the current window with the window to its left"
  (interactive)
  (swap-to-side t))

(defun swap-ends ()
  "Swap the windows at the ends of the frame"
  (interactive)
  (let ((wins (get-vert-wins)))
    (when (< 1 (length wins))
      (swap-buffers (first wins) (first (last wins))))))

(defalias 'swap 'swap-ends)

(defun rotate-windows ()
  "Rotates the windows"
  (interactive)
  (let* ((windows (get-vert-wins))
         (buffers (mapcar #'window-buffer windows))
         (buffers-swapped (append (cdr buffers) (list (car buffers)))))
    (cl-mapcar #'set-window-buffer windows buffers-swapped)))

(defun n-columns ()
  "Splits a single window into n equal sized columns"
  (interactive)
  (when (> (length (window-list)) 1)
      (throw t "must start with single window"))
  (let ((root (car (window-list)))
        (col-width (floor (/ (frame-width) 3.0))))
    (when (< col-width 72)
        (throw t "root window too small"))
    (let ((newin (split-window root col-width 'right)))
      (split-window newin col-width 'right))))

(defun three-columns ()
  "Splits a single window into three equal sized columns"
  (interactive)
  (when (> (length (window-list)) 1)
      (throw t "must start with single window"))
  (let ((root (car (window-list)))
        (col-width (floor (/ (frame-width) 3.0))))
    (when (< col-width 72)
        (throw t "root window too small"))
    (let ((newin (split-window root col-width 'right)))
      (split-window newin col-width 'right))))

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

(defun find-rel (other-win-p dir1 dir2 ext)
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

(defun find-rel-here (other-win-p file-type)
    (let* ((base (file-name-sans-extension (buffer-file-name)))
           (open-fn (if other-win-p 'find-file-other-window 'find-file))
           (exts (case file-type
                   ('c '(".c" ".cc" ".cpp"))
                   ('h '(".h" ".hpp"))
                   ('impl '("Impl.hpp"))))
           (hfiles (remove-if-not #'file-exists-p
                           (mapcar #'(lambda (x) (concat base x)) exts)))
           (the-file (car hfiles)))
      (when the-file
        (funcall open-fn the-file))))

(defun find-h (arg)
  "Look in some relative paths for the header to the file open in
the active buffer"
  (interactive "P")
  (find-rel arg "inc/" "src" ".h"))

(defun find-c (arg)
  "Look in some relative paths for the c file for the header open
in the active buffer"
  (interactive "P")
  ;(unless (find-rel arg "src/" "inc" ".c")
    (find-rel-here arg 'c))

(defun find-impl (arg)
  "Look in some relative paths for the Impl.hpp file for the file open
in the active buffer"
  (interactive "P")
  (find-rel-here arg 'impl))

(defun camelify ()
  (interactive)
  (cl-flet ((camelify-match
          (data cnt)
          (upcase (substring (match-string 0) 1 2))))
    (perform-replace "_[a-z]"
                     (cons 'camelify-match nil)
                     t t nil)))

;; (defun term-here ()
;;   (interactive)
;;   (if (not (null (getenv "DISPLAY")))
;;     (let* ((cmd (format "cd \"%s\" && exec /bin/zsh -i" default-directory))
;;       (message "DISPLAY variable is set to \'%S\'" (getenv "DISPLAY"))
;;            (args (list "-sl" "1500"
;;                        "-fn" "Consolas-14"
;;                        "-geometry" "100x35"
;;                        "-bg" "black"
;;                        "-fg" "green"
;;                        "+sb" "-e"
;;                        "zsh" "--login" "-c" cmd)))
;;       (apply 'start-process "rxvt" nil "C:/bin/rxvt.exe" args))))

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
  (beginning-of-line)
  (open-line 1))

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
  (save-excursion
    (buffer-to-ascii)
    (fill-all-paragraphs)
    (goto-char (point-min))
    (let ((counter 0))
      (insert (format "*:P%d" (incf counter)))
      (insert "\nS:")
      (forward-line)
      (while (not (eql (point) (point-max)))
        (if (looking-at "^$")
            (progn
              (incf counter)
              (while (looking-at "^$")
                (kill-line))
              (insert (format "*:P%d\nS:" counter))
              (forward-line))
          (progn
            (insert " :")
            (forward-line))))
      (goto-char (point-min))
      (insert "B:")
      (date)
      (insert "\n")
      (insert "M: [UP=RETURN_LABEL|_EXIT] \"Speed Test\"\n")
      (dotimes (i counter)
        (insert (format " :P%d \"Paragraph %d\"\n" (1+ i) (1+ i)))))))

(defun find-typing-file ()
  "Open today's typing practice file"
  (interactive)
  (let ((fname (format "news-%s.typ" (format-time-string "%Y-%m-%d"))))
    (find-file (expand-file-name (concat "~/typing/" fname)))))

(defun dbl-click (fname)
  "Run 'start' on a file, defaults to the current file"
  (interactive "ffile: ")
  (start-process "start" "*start*" "c:/bin/cygstart" fname))

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
  (let ((dname (if (equal major-mode 'dired-mode)
                   default-directory
                 (file-name-directory (buffer-file-name)))))
    (kill-new dname)))

(defun cb-fileline ()
  "Copy the basename of the current buffer's file and lineno into the clipboard"
  (interactive)
  (let ((fname (buffer-file-name))
        (lineno (line-number-at-pos)))
    (when fname
      (let ((txt (concat (file-name-nondirectory fname) ":"
                         (number-to-string lineno))))
        (kill-new txt)
        (message txt)))))

(defun cb-filename ()
  "Copy the name of the current buffer's file into the clipboard"
  (interactive)
  (let ((fname (if (equal major-mode 'dired-mode)
                   (dired-get-filename nil t)
                 (buffer-file-name))))
    (kill-new fname)))

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

(defun remove-dos-eol ()
  "Do not show ^M in files containing mixed UNIX and DOS line endings."
  (interactive)
  (setq buffer-display-table (make-display-table))
  (aset buffer-display-table ?\^M []))

(defun delete-frame-or-exit ()
  "If this is the only frame then exit, otherwise delete this frame"
  (interactive)
  (if (> (length (frame-list)) 1)
      (delete-frame)
    (if (y-or-n-p-with-timeout "Really exit Emacs? " 3 nil)
      (save-buffers-kill-emacs)
      (message ""))))

(defun cb-directory ()
  "Copy the directory of the current buffer's file into the clipboard"
  (interactive)
  (kill-new (file-name-directory (buffer-file-name))))

(defun svn-diff (arg)
  "Run svn diff on the current buffer (or directory) using winmerge"
  (interactive "P")
  (let ((target (if arg
                    (file-name-directory (buffer-file-name))
                  (buffer-file-name))))
    (start-process "svn-diff" nil
                   "svn.exe" "diff"
                   "--diff-cmd=svn-diffmerge.bat"
                   target)))

(defun toggle-tabs ()
  "Toggle between using tabs/spaces for indentation"
  (interactive)
  (setq indent-tabs-mode (not indent-tabs-mode))
  (message "tabs mode %s" (if indent-tabs-mode "on" "off")))

(defun show-window-size ()
  "Show the width of the current window in the bar"
  (interactive)
  (message (format "%dx%d" (window-body-width) (window-total-height))))

(defun hipchat-region (room)
  "Send file or region to hipchat using the /code prefix"
  (interactive "sRoom: ")
  (let* ((fname (buffer-file-name))
         (bmark (region-beginning))
         (emark (region-end))
         (bline (line-number-at-pos bmark))
         (eline (line-number-at-pos emark))
         (args (if (use-region-p)
                 (list room fname (format "%d" bline) (format "%d" eline))
               (list room fname))))
    ;(apply 'message "/home/mc/bin/hipchat-post.js %s %s %s" args)))
    (apply 'call-process "/home/mc/bin/hipchat-post.js" nil "*hipchat*" nil args)))

(defun mc-sml-find-syms-cmdline ()
  (interactive)
  (gui-set-selection
   'PRIMARY
   (concat "sml-file-line-syms.py " mc-sml-syms-file " " (buffer-file-name)
           " " (number-to-string (line-number-at-pos)))))


