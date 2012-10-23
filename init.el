;;;-*-emacs-lisp-*-
(require 'cl)

(setq hostname (replace-regexp-in-string "\\..*" "" (system-name)))
(add-to-list 'load-path (expand-file-name "~/.emacs.d/mjc"))

(require 'utils)
(require 'prefs)
(require 'fonts)
(require 'modes)
(require 'L3)
(require 'cfun-comment)

(require 'dired-x)
(require 'remember)
(when (file-exists-p (expand-file-name "~/.emacs.d/wtf.el"))
  (require 'wtf))
;(require 'bbdb)

;;; System specific
(case system-type
  ('darwin
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

(server-start)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(wtf-custom-alist (quote (("CCB" . "change control board") ("IA" . "information assurance") ("IPT" . "integrated product team") ("ODSA" . "operational data storage and analysis") ("ROM" . "rough order of magnitude")))))
