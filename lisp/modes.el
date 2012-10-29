;; Major mode hooks
; Info directory path
(add-hook 'Info-mode-hook
          (lambda ()
            (dolist (p (list (file-truename  (concat invocation-directory "../info/"))
                             (expand-file-name "~/sw/share/info")
                             "C:/cygwin/usr/share/info"
                             "C:/usr/share/info"
                             "/usr/share/info"
                             "/usr/local/share/info"
                             (expand-file-name "~/sw/usr/share/info")))
              (when (file-exists-p p)
                  (add-to-list 'Info-directory-list (file-truename p) t)))))

; Make calendar window a dedicated window
; (might be better to tweak the window selection algorithm
;  variables so that buffers don't open in tiny windows)
(add-hook 'calendar-initial-window-hook
  (lambda ()
    (let ((calwins (get-buffer-window-list "*Calendar*")))
      (when (= (length calwins) 1)
        (set-window-dedicated-p (first calwins) t)))
    (define-key calendar-mode-map "q" 'delete-window)))

;;; Various programming modes and settings
(which-function-mode 1)
(add-to-list 'which-func-modes 'lisp-mode)

(add-to-list 'auto-mode-alist '("\\.asd$" . lisp-mode))
(add-to-list 'auto-mode-alist '("\\.h$" . c++-mode))
(dolist (subdir (list "ecb" "ocaml" "slime"))
  (let ((dirname (expand-file-name (concat "~/.emacs.d/" subdir))))
    (when (file-exists-p dirname)
      (add-to-list 'load-path dirname))))

(defun mjc-info-load-hook ()
  (let ((d (expand-file-name "~/sw/share/info")))
    (when (file-exists-p d)
      (add-to-list 'Info-directory-list d))))

(when (file-exists-p (expand-file-name "~/.emacs.d/js2.el"))
  (autoload 'js2-mode "js2" nil t)
  (add-to-list 'auto-mode-alist '("\\.js$" . js2-mode)))

(defun setup-ocaml ()
  (interactive)
  (add-to-list 'auto-mode-alist '("\\.ml[iyl]?$" . caml-mode))
  (require 'ocaml)
  (autoload 'caml-mode "ocaml" (interactive)
    "Major mode for editing oCaml code." t)
  (autoload 'camldebug "camldebug" (interactive) "Debug caml mode"))

(defun setup-racket ()
  (interactive)
  (load-file (expand-file-name "~/.emacs.d/geiser-0.1.3/elisp/geiser.el"))
  (setq geiser-racket-binary (expand-file-name "~/sw/Racket v5.2/bin/racket")))

;(setenv "SBCL_HOME" "/Users/mjc/sw/lib/sbcl")
;(load (expand-file-name "~/quicklisp/slime-helper.el"))
(when (file-exists-p (expand-file-name  "~/.emacs.d/slime"))
  (let* ((possible-lisp-locations
          (list
           (expand-file-name "~/sw/sbcl/bin/sbcl")
           "C:/PROGRA~2/clisp-2.49/clisp.exe"
           "C:/PROGRA~2/STEELB~1/101F4D~1.53/sbcl.exe"))
         (available-lisps
          (remove-if-not #'file-exists-p possible-lisp-locations)))
    (when available-lisps
      (setq inferior-lisp-program (first available-lisps))
      (require 'slime)
      (setf slime-startup-animation nil)
      (setf common-lisp-hyperspec-root "http://localhost/~mjc/HyperSpec/")
      (slime-setup '(slime-repl)))))

(when (file-exists-p (expand-file-name "~/.emacs.d/d-mode.el"))
  (require 'd-mode)
  (add-to-list 'auto-mode-alist '("\\.d$" . d-mode)))

(defun setup-g-lib ()
  (interactive)
  (load-library "g")
  (setq g-user-email "mike.casinghino@gmail.com"))

(defun setup-jdee ()
  (interactive)
  (if (file-exists-p (expand-file-name "~/.emacs.d/jdee-2.4.0.1"))
      (progn
        (add-to-list 'load-path (expand-file-name "~/.emacs.d/jdee-2.4.0.1/lisp"))
        (add-to-list 'load-path (expand-file-name "~/.emacs.d/cedet-1.0/common"))
        (load-file (expand-file-name "~/.emacs.d/cedet-1.0/common/cedet.el"))
        (add-to-list 'load-path (expand-file-name "~/.emacs.d/elib-1.0"))
        (require 'jde))
    (message "Couldn't find directory %s" (expand-file-name "~/.emacs.d/jdee-2.4.0.1"))))

;; Mode Hooks
(setq c-default-style '((java-mode . "java")
                        (awk-mode . "awk")
                        (other . "k&r")))

(defun mjc-c-mode-common-hook ()
  (setq comment-start "//")
  (setq comment-end "")
  (setq tab-width 4)
  (setq c-basic-offset 4))

(defun mjc-c++-mode-hook ()
  (c-set-style "stroustrup")
  (c-set-offset 'innamespace 0)

(defun mjc-d-mode-hook ()
  (whitespace-mode 1)
  (setq tab-width 4)
  (setq c-basic-offset 4)
  (setq comment-start "//")
  (setq comment-end ""))

(add-hook 'c-mode-common-hook 'mjc-c-mode-common-hook)
(add-hook 'c++-mode-hook 'mjc-c++-mode-hook)
(add-hook 'd-mode-hook 'mjc-d-mode-hook)

(when (file-exists-p (expand-file-name "~/.emacs.d/cc-mode"))
  (add-to-list 'load-path (expand-file-name "~/.emacs.d/cc-mode"))
  (require 'cc-mode))

(provide 'modes)
