(defun main ()
  ;(print "Creating imported classes...")
  ;(create-all-import-classes) Tohle případně slouží k vytvoření komentářů a vah, i když by toho měly asi taky být součástí
  (print "Downloading missing files...")
  (redownload "/")
  (print "Building core text database...")
  (terpri)
  (build-core-text-database)
  (print "Reading data...")
  (refresh-imports)
  (read-saved)
  (read-config)
  (build-subclasses)
  (print "Building corpora...")
  (rebuild-corpus) ; Is that needed?
  (print "All done. Opening classifier.")
  (run))
