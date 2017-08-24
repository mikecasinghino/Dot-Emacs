(setf dbnet-file-types
      '((impl "Impl\\.hpp$")
        (impl "Impl\\.hh$")
        (cpp "\\.cc$")
        (hpp "\\.hpp$")
        (hpp "\\.hh$")
        (js "\\.js$")))

(setf dbnet-file-type-cycle-all
      '((cpp ".hpp" "Impl.hpp")
        (hpp "Impl.hpp" ".cc")
        (impl ".cc" ".hpp")))

(setf dbnet-file-type-cycle-h
      '((cpp  ".hpp")
        (hpp  ".hpp")
        (impl ".hpp")))

(setf dbnet-file-type-cycle-impl
      '((cpp  "Impl.hpp")
        (hpp  "Impl.hpp")
        (impl "Impl.hpp")))

(setf dbnet-file-type-cycle-c
      '((hpp  ".cc")
        (impl ".cc")
        (cpp  ".cc")))

(defun dbnet-lgrep-aliases ()
  "Add *.cc *.hpp to grep-files-aliases"
  (interactive)
  (add-to-list 'grep-files-aliases '("dbnet" . "*.cc *.hpp"))
  (add-to-list 'grep-files-aliases '("dbnet-sml" . "*.sig *.sml")))

(defun dbnet-fix-semantic ()
  "Add sml files to semantic-symref-filepattern-alist"
  (interactive)
  (push '(sml-mode "*.sml" "*.sig" "*.mldb" "*.mlb") semantic-symref-filepattern-alist))

(defun buffer-file-name-to-type (types)
  "Uses the give list of '((type <regexp>) ...) to determine the file type"
  (let ((ret) (stem)
        (fname (buffer-file-name)))
    (while (and types (not ret))
      (let* ((elt (car types))
            (mpos (string-match (car (last elt)) fname)))
        (when mpos
          (setf ret (car elt))
          (setf stem (substring fname 0 mpos)))
        (setf types (cdr types))))
    (list ret stem)))

(defun dbnet-file-type ()
  (buffer-file-name-to-type dbnet-file-types))

(defun dbnet-find-file-in (stem srch-lst)
  (when srch-lst
    (let* ((end (car srch-lst))
           (next-file (concat stem end)))
      (if (file-readable-p next-file)
           (find-file next-file)
        (dbnet-find-file-in stem (cdr srch-lst))))))

(defun dbnet-cycle-file-type (cycle)
  (destructuring-bind (ftype stem) (dbnet-file-type)
    (let ((ends (cdr (assoc ftype cycle))))
         (dbnet-find-file-in stem ends))))

(defun dbnet-h-file ()
  (interactive)
  (dbnet-cycle-file-type dbnet-file-type-cycle-h))

(defun dbnet-impl-file ()
  (interactive)
  (dbnet-cycle-file-type dbnet-file-type-cycle-impl))

(defun dbnet-c-file ()
  (interactive)
  (dbnet-cycle-file-type dbnet-file-type-cycle-c))

(defun dbnet-build-dbfw ()
  (interactive)
  (if (getenv "TARADIR")
      (compile "cd $TARADIR/obj/dbfw && build dbfw")
    (message "TARADIR is not set")))

(defun dbnet-switch-to-file-ext (ext)
  (let ((bfn (buffer-file-name)))
    (when bfn
      (let ((sigfname (concat (file-name-sans-extension bfn) ext)))
        (if (file-exists-p sigfname)
            (find-file sigfname)
          (message (format "no such file %s" signfame)))))))

(defun dbnet-sig-file ()
  "Switch to the .sig file for this .sml file"
  (interactive)
  (dbnet-switch-to-file-ext ".sig"))

(defun dbnet-sml-file ()
  "Switch to the .sml file for this .sig file"
  (interactive)
  (dbnet-switch-to-file-ext ".sml"))

(defun dbnet-sml-mode-hook ()
  (local-set-key (kbd "C-c i") 'dbnet-sig-file)
  (local-set-key (kbd "C-c s") 'dbnet-sml-file))

(defun dbnet-c++-mode-hook ()
  (local-set-key (kbd "C-c h") 'dbnet-h-file)
  (local-set-key (kbd "C-c j") 'dbnet-impl-file)
  (local-set-key (kbd "C-c k") 'dbnet-c-file)
  (local-set-key (kbd "C-c u") 'uncomment-region)
  (local-set-key (kbd "<f7>") 'dbnet-build-dbfw)
  (local-unset-key (kbd "C-c C-u")))

(add-hook 'c++-mode-hook 'dbnet-c++-mode-hook)
(eval-after-load 'grep '(dbnet-lgrep-aliases))
;(setq mc-sml-syms-file "/home/mc/wrk/dbfx/obj/reaction/tally_dispatcher.syms")
