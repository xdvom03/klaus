(defun imports ()
  (mapcar #'namestring (directory (concat *imports-folder* "*/"))))

(defun process-imported-corpora (assoc-corpora)
  ;; Reads it in assoc list format
  (assoc-to-hashtable (mapcar #'(lambda (cons)
                                  (cons (car cons)
                                        (assoc-to-hashtable (cdr cons))))
                              assoc-corpora)))

(defun saveable-corpora (hashtable-corpora)
  (hashtable-to-assoc (map-to-hash (compose #'sort-corpus
                                            #'hashtable-to-assoc
                                            #'(lambda (class)
                                                (gethash class hashtable-corpora)))
                                   (list-keys hashtable-corpora))))

(let ((import-corpora (ht))
      (import-urls (ht)))
  (defun ignore-imports ()
    "Sets imports in memory to none, but keeps the files. Used for viewing crawler results wthout import contamination."
    (setf import-urls (ht))
    (setf import-corpora (ht)))
  
  (defun refresh-imports ()
    (setf import-urls (map-to-hash #'(lambda (import-folder)
                                       (assoc-to-hashtable (read-from-file (concat import-folder "urls"))))
                                   (imports)))
    (setf import-corpora (map-to-hash #'(lambda (import-folder)
                                          (process-imported-corpora (read-from-file (concat import-folder "corpora"))))
                                      (imports))))

  (defun save-imports ()
    ;; necessary because of file system editing
    (dolist (import-folder (list-keys import-urls))
      (let ((urls-file (concat import-folder "urls"))
            (corpora-file (concat import-folder "corpora")))
        (overwrite-file urls-file (hashtable-to-assoc (gethash import-folder import-urls)))
        (overwrite-file corpora-file (saveable-corpora (gethash import-folder import-corpora))))))

  (defun move-class-imports (old-path new-path)
    (dolist (import-class (list-keys import-urls))
      (move-hash (gethash import-class import-urls) old-path new-path)
      (move-hash (gethash import-class import-corpora) old-path new-path)))
  
  (defun imported-corpus (class)
    (reduce #'add-corpuses (mapcar #'(lambda (import)
                                       (fallback (gethash class import)
                                                 (ht)))
                                   (list-values import-corpora))
            :initial-value (ht)))
  
  (defun imported-file-count (class)
    (reduce #'+ (mapcar #'(lambda (tree)
                            (length (gethash class tree)))
                        (list-values import-urls))
            :initial-value 0))

  (defun imported-urls (class)
    (reduce #'append (mapcar #'(lambda (tree)
                                 (gethash class tree))
                             (list-values import-urls))
            :initial-value nil))

  (defun recursive-imported-urls (class)
    (reduce #'append (append1 (mapcar #'recursive-imported-urls (subclasses class))
                              (imported-urls class))
            :initial-value nil))

  (defun imported-classes ()
    (remove-duplicates (reduce #'append (mapcar #'list-keys (list-values import-urls))
                               :initial-value nil)
                       :test #'equal)))
