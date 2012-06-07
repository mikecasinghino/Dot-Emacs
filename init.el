;;;-*-emacs-lisp-*-
(require 'cl)
(server-start)

;;; Font selection and colors
(condition-case nil
    (let ((my-fonts
           '("DejaVu Sans Mono-8"
             "Bitstream Vera Sans Mono-8"
             "Consolas-8"
             "Mensch-8")))
      (flet ((set-first-font (fonts)
               (cond
                ((null fonts) nil)
                ((x-list-fonts (car fonts))
                 (set-default-font (car fonts)))
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

; Not quite a theme...
(set-background-color "black")
(set-foreground-color "green")
(set-cursor-color "green")
(setq visible-bell t)
(setq default-frame-alist
      (append default-frame-alist
              `((background-color . "black")
                (foreground-color . "green")
                (cursor-color . "green")
                (font . ,(get-current-font)))))

(require 'dired-x)
(require 'remember)
;(require 'wtf)
;(require 'bbdb)

;;; Preferences
(setq inhibit-splash-screen t)
(setq initial-scratch-message "")
(line-number-mode 1)
(column-number-mode 1)
(ido-mode t)
(put 'narrow-to-region 'disabled nil)
(setq-default make-backup-files nil)
(setq-default indent-tabs-mode nil)
(setq-default require-final-newline t)
(recentf-mode 1)
(setq recentf-max-saved-items 120)
(blink-cursor-mode 1)
(setq bookmark-default-file (expand-file-name "~/.emacs.d/bookmarks"))
(setq bookmark-save-flag 1)
(windmove-default-keybindings)
(mouse-avoidance-mode 'jump)
(setq erc-hide-list '("JOIN" "PART" "QUIT"))
(add-to-list 'auto-mode-alist '("\\.org$" . org-mode))
(add-to-list 'auto-mode-alist '("\\.rkt$" . scheme-mode))
(setq calendar-latitude 32.7)
(setq calendar-longitude -117.1)
(setq calendar-location-name "San Diego, CA")
(setq calendar-mark-holidays-flag t)
(setq calendar-mark-diary-entries-flag t)
;(bbdb-initialize)
(setq whitespace-line-column 100)
(setq whitespace-style
      '(face trailing lines-tail empty tab-mark))
(whitespace-mode 1)
(put 'scroll-left 'disabled nil)

(defmacro safe-off (mode)
  "Call the function mode with arg -1 if it is fboundp"
  `(if (fboundp ',mode)
       (,mode -1)))

(safe-off scroll-bar-mode)
(safe-off tool-bar-mode)
(safe-off menu-bar-mode)
(safe-off tooltip-mode)

(global-set-key "\C-w" 'backward-kill-word)
(global-set-key "\C-x\C-k" 'kill-region)
(global-set-key "\C-xY" 'copy-region-as-kill)
(global-set-key "\C-x1" 'delete-other-windows-vertically)
(global-set-key "\C-x!" 'delete-other-windows)
(global-set-key "\C-x\C-c" 'delete-frame)
; execute save-buffers-kill-emacs to really exit

;;; Aliases
(defalias 'cb 'clipboard-kill-ring-save)
(defalias 'cb-yank 'clipboard-yank)
(defalias 'dml 'delete-matching-lines)
(defalias 'ffp 'find-file-at-point)
(defalias 'flm 'font-lock-mode)
(defalias 'lml 'list-matching-lines)
(defalias 'qr 'query-replace)
(defalias 'qrr 'query-replace-regexp)
(defalias 'rof 'recentf-open-files)
(defalias 'rr 'replace-regexp)
(defalias 'dtw 'delete-trailing-whitespace)
(defalias 'stw 'toggle-show-trailing-whitespace)
(defalias 'ttl 'toggle-truncate-lines)
(defalias 'wsm 'whitespace-mode)
(defalias 'sbke 'save-buffers-kill-emacs)
(defun toggle-tabs ()
  "Toggle between using tabs/spaces for indentation"
  (interactive)
  (setq indent-tabs-mode (not indent-tabs-mode))
  (message "tabs mode %s" (if indent-tabs-mode "on" "off")))

;;; System specific
(case system-type
  ('darwin
   (setenv "SBCL_HOME" (expand-file-name "~/sw/sbcl/lib/sbcl"))
   (setq ns-command-modifier 'meta)
   (setq ns-right-alternate-modifier 'control)
   (if (fboundp 'menu-bar-mode)
       (menu-bar-mode nil))
   (global-set-key "\M-`" 'other-frame))
  ('gnu/linux)
  ('windows-nt
   (setenv "DISPLAY" nil)
   (setq split-height-threshold 100)
   (when (file-exists-p "C:/bin")
     (setenv "PATH" (concat "C:\\bin" path-separator (getenv "PATH")))
     (add-to-list 'exec-path "c:/bin")))
  (t
   (warn (format "System type '%a' not recognized" system-type))))

;; Major mode hooks
; Info directory path
(add-hook 'Info-mode-hook
          (lambda ()
            (dolist (p (list "C:/usr/share/info"
                             "/usr/share/info"
                             "/usr/local/share/info"
                             (expand-file-name "~/sw/usr/share/info")))
              (when (file-exists-p p)
                  (add-to-list 'Info-directory-list p t)))))

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

(defun make-dir-file (base part)
  (expand-file-name
   (concat (file-name-as-directory base)
           part)))

(defun emacs-dir-file (part) (make-dir-file "~/.emacs.d" part))
(defun home-dir-file (part) (make-dir-file "~" part))

(add-to-list 'load-path (emacs-dir-file "ecb"))
;(require 'ecb)

(when (file-exists-p (expand-file-name (emacs-dir-file "js2.el")))
  (autoload 'js2-mode "js2" nil t)
  (add-to-list 'auto-mode-alist '("\\.js$" . js2-mode)))

(defun setup-ocaml ()
  (interactive)
  (add-to-list 'load-path (emacs-dir-file "ocaml"))
  (add-to-list 'auto-mode-alist '("\\.ml[iyl]?$" . caml-mode))
  (require 'ocaml)
  (autoload 'caml-mode "ocaml" (interactive)
    "Major mode for editing oCaml code." t)
  (autoload 'camldebug "camldebug" (interactive) "Debug caml mode"))

(defun setup-racket ()
  (interactive)
  (load-file (emacs-dir-file "geiser-0.1.3/elisp/geiser.el"))
  (setq geiser-racket-binary (home-dir-file "sw/Racket v5.2/bin/racket")))

(when (file-exists-p (emacs-dir-file "slime"))
  (let* ((possible-lisp-locations
          (list
           (expand-file-name "~/sw/sbcl/bin/sbcl")
           "C:/PROGRA~2/clisp-2.49/clisp.exe"
           "C:/PROGRA~2/STEELB~1/101F4D~1.53/sbcl.exe"))
         (available-lisps
          (remove-if-not #'file-exists-p possible-lisp-locations)))
    (when available-lisps
      (add-to-list 'load-path (emacs-dir-file "slime"))
      (setq inferior-lisp-program (first available-lisps))
      (require 'slime)
      (setf slime-startup-animation nil)
      (setf common-lisp-hyperspec-root "http://localhost/~mjc/HyperSpec/")
      (slime-setup '(slime-repl)))))

(when (file-exists-p (emacs-dir-file "d-mode.el"))
  (require 'd-mode)
  (add-to-list 'auto-mode-alist '("\\.d$" . d-mode)))

(defun setup-g-lib ()
  (interactive)
  (load-library "g")
  (setq g-user-email "mike.casinghino@gmail.com"))

(defun setup-jdee ()
  (interactive)
  (if (file-exists-p (emacs-dir-file "jdee-2.4.0.1"))
      (progn
        (add-to-list 'load-path (emacs-dir-file "jdee-2.4.0.1/lisp"))
        (add-to-list 'load-path (emacs-dir-file "cedet-1.0/common"))
        (load-file (emacs-dir-file "cedet-1.0/common/cedet.el"))
        (add-to-list 'load-path (emacs-dir-file "elib-1.0"))
        (require 'jde))
    (message "Couldn't find directory %s" (emacs-dir-file "jdee-2.4.0.1"))))

;; Mode Hooks
(setq c-default-style '((java-mode . "java")
                        (awk-mode . "awk")
                        (other . "k&r")))

(defun mjc-c-mode-common-hook ()
  (setq comment-start "//")
  (setq comment-end "")
  (setq tab-width 4)
  (setq c-basic-offset 4))

(defun incontrol-src-file-p (fname)
  (string-match "InControl-Build" fname))

(defun mjc-c++-mode-hook ()
  (c-set-style "stroustrup")
  (c-set-offset 'innamespace 0)
  (when t
    (dolist (item '((inclass ++)
                    (access-label -)
                    (case-label +)
                    (inline-open 0)))
      (apply 'c-set-offset item))))

(defun mjc-d-mode-hook ()
  (whitespace-mode 1)
  (setq tab-width 4)
  (setq c-basic-offset 4)
  (setq comment-start "//")
  (setq comment-end ""))

(add-hook 'c-mode-common-hook 'mjc-c-mode-common-hook)
(add-hook 'c++-mode-hook 'mjc-c++-mode-hook)
(add-hook 'd-mode-hook 'mjc-d-mode-hook)

(when (file-exists-p (emacs-dir-file "cc-mode"))
  (add-to-list 'load-path (emacs-dir-file "cc-mode"))
  (require 'cc-mode))

;; My code
(add-to-list 'load-path (emacs-dir-file ""))
(add-to-list 'load-path (emacs-dir-file "mjc"))

(require 'mjc-utils)
(require 'mjc-L3)
(require 'cfun-comment)

;; These are in mjc-utils
(global-set-key "\C-c*" 'forward-next-word-under-point)
(global-set-key "\C-c#" 'backward-next-word-under-point)
