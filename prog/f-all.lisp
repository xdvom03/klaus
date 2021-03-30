(dolist (file (directory "../prog/*.lisp"))
  (if (not (or (search "all" (namestring file))
               ;; if a file is being edited
               (search "#" (namestring file))))
      (load (print file))))
