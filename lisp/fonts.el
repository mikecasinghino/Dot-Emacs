;;; Font selection and colors
(condition-case nil
    (let ((my-fonts
           '("DejaVu Sans Mono-12"
             "Bitstream Vera Sans Mono-8"
             "Consolas-10"
             "Mensch-10")))
      (flet ((set-first-font (fonts)
               (cond
                ((null fonts) nil)
                ((x-list-fonts (car fonts))
                 (set-default-font (car fonts) nil t))
                (t
                 (set-first-font (cdr fonts))))))
        (set-first-font my-fonts)))
  (error nil))
; Test: ~ _ - . , 0 O 1 i l | ! ` '

(defun get-current-font ()
  (let ((pair (assoc 'font (frame-parameters))))
    (if (null pair) nil
      (cdr pair))))

(defun show-current-font ()
  (interactive)
  (message "%S" (get-current-font)))
