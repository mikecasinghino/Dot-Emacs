;; (add-to-list
;;  'display-buffer-alist
;;  `(,(rx bos "*compilation*" eos)
;;    (display-buffer-reuse-window
;;     display-buffer-in-side-window)
;;    (side . right)))

(defun dbnet-geometry ()
  (interactive)
  (set-frame-position (selected-frame) 1922 0)
  (set-frame-size (selected-frame) 359 105))

(defun show-frame-geometry ()
  (interactive)
  (let ((left (cdr (assoc 'left (frame-parameters))))
        (top (cdr (assoc 'top (frame-parameters))))
        (width (cdr (assoc 'width (frame-parameters))))
        (height (cdr (assoc 'height (frame-parameters)))))
    (message (format "%dx%d+%d+%d" width height left top))))

(defun reset-geometry ()
  (interactive)
  (set-frame-position (selected-frame) 0 0)
  (set-frame-size (selected-frame) 100 100))
