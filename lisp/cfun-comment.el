(defun io-from-param-string (param-string)
  (if (string-match "\\bconst\\b" param-string)
      'IN
    (if (or (string-match "&" param-string)
            (string-match "*" param-string))
        'OUT
      'IN)))

(defun string-trim (str)
  (let ((beg (string-match "\\w" str))
        (end (string-match "\\s-+$" str)))
    (substring str (or beg 0) (or end '()))))

(defun cleanup-c-name (cname)
  (let ((start-index (string-match "[a-zA-Z_]" cname)))
    (let ((end-index (string-match "[^a-zA-Z0-9_]" cname start-index)))
      (if (eq start-index nil)
          cname
        (string-trim (substring cname start-index end-index))))))

(defun param-string-to-name-type (param-string)
  (let ((name-index (string-match "\\w+\\(\\s-+UNUSED\\)?$" param-string)))
    (list (cleanup-c-name (substring param-string 0 name-index))
          (cleanup-c-name (substring param-string name-index)))))

(defun param-to-tuple (param)
  (let ((c-name-type (param-string-to-name-type param)))
    (list (car c-name-type)
          (cadr c-name-type)
          (io-from-param-string param))))

(defun c-extract-params (str)
  "Given a C parameter list, return a list of name, inout tuple for each param"
  (let ((params (split-string (string-trim str) ","))
        (tuples '()))
    (dolist (param params tuples)
      (let ((tup (param-to-tuple param)))
        (setq tuples (cons tup tuples))))))

(defun cur-word ()
  (interactive)
  (message "curword: %s" (current-word)))

(defun is-blank-line ()
  (save-excursion
    (beginning-of-line)
    (let ((beg (point)))
         (end-of-line)
         (string-match "^\\s-*$" (buffer-substring beg (point))))))

(defun buffer-extract-to (chr)
  (save-excursion
    (let ((beg (point)))
      (re-search-forward (regexp-quote chr))
      (or (buffer-substring (+ beg 1) (- (point) 1)) ""))))

(defun get-fname ()
  (let ((fname (current-word)))
    (save-excursion
      (backward-word 1)
      (backward-char 1)
      (if (looking-at ":\\|~")
          (progn
            (forward-char 1)
            (let ((end (point)))
              (backward-word 1)
              (concat (buffer-substring (point) end) fname)))
        fname))))

(defun l3-comment-function ()
  "Find the next function signature and generate a comment template"
  (interactive)
  (if (not (re-search-forward "^\\w[^(]*("))
      (message "couldn't find function beginning")
    (backward-char)
    (let ((lparen (point))
          (fname (get-fname))
          (rtype nil)
          (comment-insertion-point nil)
          (csig nil))
      (previous-line 1)
      (beginning-of-line)
      (if (is-blank-line)
          (progn
            (setq rtype nil)
            (next-line 1)
            (setq comment-insertion-point (point)))
        (progn
          (end-of-line)
          (let ((cw (current-word)))
            (if (string-equal cw "void")
                (setq rtype nil)
              (setq rtype cw))
            (beginning-of-line)
            (setq comment-insertion-point (point)))))
      (goto-char lparen)
      (setq csig (c-extract-params (buffer-extract-to ")")))
      (goto-char comment-insertion-point)
      (insert l3-comment-function-template)
      (search-backward "FUNCTION:")
      (end-of-line)
      (insert fname)
      (search-forward "PARAMETERS:")
      (if (not (null csig))
          (dolist (tup (reverse csig))
            (insert (format "\n *   %s: %s - " (cadr tup) (car (last tup)))))
        (insert (format "\n *   N/A")))
      (let ((rtype-str (if rtype (concat rtype " - " ) "N/A")))
        (search-forward "RETURN VALUES:")
        (insert (format "\n *   %s" rtype-str)))
      (search-backward "DESCRIPTION:")
      (next-line 1)
      (forward-char 2))))

(setq l3-comment-function-template
"
/**------------------------------------------------------------------------
 *
 * FUNCTION: 
 *
 * DESCRIPTION:
 *    
 *
 * PARAMETERS:
 *
 * RETURN VALUES:
 *
 * VISIBILITY: none
 *
 *-------------------------------------------------------------------------*/
"
)

(provide 'cfun-comment)