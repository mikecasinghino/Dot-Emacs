(add-to-list
 'display-buffer-alist
 `(,(rx bos "*compilation*" eos)
   (display-buffer-reuse-window
    display-buffer-in-side-window)
   (side . right)))
