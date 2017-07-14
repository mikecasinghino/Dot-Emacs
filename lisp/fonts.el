;;; Font selection and colors

(defun mc-get-current-font ()
  (let ((pair (assoc 'font (frame-parameters))))
    (if (null pair) nil
      (cdr pair))))

(defun show-current-font ()
  (interactive)
  (message "%S" (mc-get-current-font)))

(setf mc-preferred-fonts
      '("Ubuntu Mono-8"
        "Menlo Regular-12"
        "DejaVu Sans Mono-8"
        "Bitstream Vera Sans Mono-8"
        "Consolas-10"
        "Mensch-10"))

;; This really should be in Xresources
(defun mc-select-best-font ()
  (interactive)
  (let* ((fnts (mapcan #'x-list-fonts mc-preferred-fonts))
         (norms (remove-if
                 #'(lambda (f) (or (search "-bold-" f) (search "-oblique-" f)))
                 fnts))
         (font (car-safe norms)))
    (when font
      (if (set-frame-font font)
          (messsage "font set to %s" font)
        (message "unable to set font to %s" font)))))
                                        ; Test: ~ _ - . , 0 O 1 i l | ! ` '
(defun mc-set-best-font ()
  (interactive)
  (let ((w (frame-pixel-width))
        (h (frame-pixel-height)))
    (mc-select-best-font)
    (set-frame-size (selected-frame) w h t)))

(defun mc-change-font-size (&optional dec)
  (let ((parts (cdr (split-string (mc-get-current-font) "-"))))
    (if (> (length parts) 7)
        (let* ((cursz (string-to-number (nth 6 parts)))
             (newsz (number-to-string (if dec (1- cursz) (1+ cursz))))
             (pfx (subseq parts 0 6))
             (sfx (subseq parts 7))
             (newlist (append (reverse (cons newsz (reverse pfx))) sfx)))
        ;(message "found parts %s" parts)))))
          (set-frame-font (concat "-" (mapconcat 'identity newlist "-"))))
      (progn
        (message "doesn't appear to be an X11 frame")
        nil))))

(defun mc-inc-font-size ()
  (interactive)
  (let ((w (frame-pixel-width))
        (h (frame-pixel-height)))
    (mc-change-font-size)
    (set-frame-size (selected-frame) w h t)))

(defun mc-dec-font-size ()
  (interactive)
  (let ((w (frame-pixel-width))
        (h (frame-pixel-height)))
    (mc-change-font-size t)
    (set-frame-size (selected-frame) w h t)))

;(mc-set-best-font)
(global-set-key (kbd "C-+") 'mc-inc-font-size)
