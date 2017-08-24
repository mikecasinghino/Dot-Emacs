;;; Font selection and colors

(defun mc-get-current-font ()
  (let ((pair (assoc 'font (frame-parameters))))
    (if (null pair) nil
      (cdr pair))))

(defun show-current-font ()
  (interactive)
  (message "%S" (mc-get-current-font)))

(defun mc-make-font-filename (base)
  (expand-file-name (concat "~/.emacs.d/lisp/" base ".fonts")))

(defun mc-read-font-file (fname)
  (with-temp-buffer
    (insert-file-contents fname)
    (read (buffer-string))))

(defun mc-read-fonts ()
  (let ((system-font-file (mc-make-font-filename (system-name))))
    (if (file-readable-p system-font-file)
        (mc-read-font-file system-font-file)
      (mc-read-font-file (mc-make-font-filename "defaults")))))

;; This really should be in Xresources
(defun mc-select-best-font ()
  (interactive)
  (let* ((fnts (mapcan #'x-list-fonts (mc-read-fonts)))
         (norms (remove-if
                 #'(lambda (f) (or
                                (search "-bold-" f)
                                (search "-oblique-" f)
                                (search "-italic" f)))
                 fnts))
         (font (car-safe norms)))
    (if font
      (if (set-frame-font font)
          (messsage "font set to %s" font)
        (message "unable to set font to %s" font))
      (message "couldn't find a font"))))
                                        ; Test: ~ _ - . , 0 O 1 i l | ! ` '
(defun mc-set-best-font ()
  (interactive)
  (let ((w (frame-pixel-width))
        (h (frame-pixel-height)))
    (mc-select-best-font)
    (set-frame-size (selected-frame) w h t)))
