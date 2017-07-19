;;;-*-emacs-lisp-*-
(require 'cl)

(require 'package)
(add-to-list 'package-archives
             '("melpa-stable" . "http://stable.melpa.org/packages/") t)
(package-initialize)

(load-theme 'tango-dark)

(setq hostname (downcase (replace-regexp-in-string "\\..*" "" (system-name))))
(add-to-list 'load-path (expand-file-name "~/.emacs.d/3rd"))
(add-to-list 'load-path (expand-file-name "~/.emacs.d/lisp"))

(load "utils")
(load "prefs")
(load "fonts")
(load "modes")
(load "dbnet")

(custom-set-variables
 '(org-replace-disputed-keys t))

(defun post-init-stuff () )

(add-hook 'after-init-hook (lambda () (post-init-stuff)))

(add-hook 'dired-load-hook
          (function (lambda () (load "dired-x"))))

;(require 'key-chord)
(require 'remember)
(when (file-exists-p (expand-file-name "~/.emacs.d/wtf.el"))
  (require 'wtf))
;(require 'bbdb)

;;; System specific
(case system-type
  ('darwin
   (mc-set-best-font)
   (setq ns-command-modifier 'meta)
   (setq ns-right-alternate-modifier 'control)
   (when (file-exists-p (expand-file-name "~/sw/bin"))
     (add-to-list 'exec-path (expand-file-name "~/sw/bin")))
     ;; (setf eshell-path-env (concat eshell-path-env ":"
     ;;                               (expand-file-name "~/sw/bin"))))
   (global-set-key "\M-n" 'make-frame)
   (if (fboundp 'menu-bar-mode)
       (menu-bar-mode nil))
   (global-set-key "\M-`" 'other-frame))
  ('gnu/linux)
  ('windows-nt
   (setenv "DISPLAY" nil)
   (setq split-height-threshold 100)
   (when (file-exists-p "C:/bin")
     (setenv "PATH" (concat "C:\\bin" path-separator (getenv "PATH")))
     (add-to-list 'exec-path "c:/bin"))
   (when (file-exists-p "C:/cygwin/bin")
     (setenv "PATH" (concat "C:\\cygwin\\bin" path-separator (getenv "PATH")))
     (add-to-list 'exec-path "c:/cygwin/bin")))
  (t
   (warn (format "System type '%a' not recognized" system-type))))

(let ((local-lisp-file (expand-file-name (format "~/.emacs.d/lisp/%s" hostname))))
  (when (file-exists-p (concat local-lisp-file ".el"))
    (load local-lisp-file)))

(server-start)
(setq custom-file "~/.emacs.d/custom.el")
(global-auto-revert-mode 1)
