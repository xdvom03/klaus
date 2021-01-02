(dolist (file (directory "../prog/*.lisp"))
  (if (not (or (search "all" (namestring file))
               (search "zzz" (namestring file))))
      (load file)))

(run)
