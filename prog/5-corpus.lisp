(defun occurrences (word corpus)
  (fallback (gethash word corpus) 0))

(defun word-count (corpus)
  (let ((vocab (list-keys corpus))
        (acc 0))
    (dolist (word vocab)
      (incf acc (occurrences word corpus)))
    acc))

(defun scale-corpus (corp num)
  (map-to-hash #'(lambda (word)
                   (* num (occurrences word corp)))
               (list-keys corp)))

(defun add-corpuses (corp1 corp2)
  (map-to-hash #'(lambda (word) (+ (occurrences word corp1)
                                   (occurrences word corp2)))
               (remove-duplicates (append (list-keys corp1)
                                          (list-keys corp2))
                                  :test #'equal)))

(defun sort-corpus (corp)
  ;; Only applicable to the list format of corpuses.
  (sort (copy-seq corp) #'< :key #'cdr))

;;; UTILS
;;;----------------------------------------------------------------------------------------------
;;; IMPORT/EXPORT

(defun imports ()
  (mapcar #'namestring (directory (concat *imports-folder* "*/"))))

(let (url-trees
      assoc-import-corpora
      import-corpora
      import-urls)
  (defun refresh-imports ()
    (let ((data (mapcar #'(lambda (import-folder)
                            (cons (read-from-file (concat import-folder "urls"))
                                  (read-from-file (concat import-folder "corpora"))))
                        (imports))))
      (setf url-trees (mapcar #'car data))
      (setf assoc-import-corpora (mapcar #'cdr data))

      (setf import-corpora (mapcar (compose #'assoc-to-hashtable
                                            #'(lambda (assoc)
                                                (mapcar #'(lambda (cons)
                                                            (cons (car cons)
                                                                  (assoc-to-hashtable (cdr cons))))
                                                        assoc)))
                                   assoc-import-corpora))
      (setf import-urls (mapcar #'assoc-to-hashtable
                                url-trees))))
  
  (defun imported-corpus (class)
    (reduce #'add-corpuses (mapcar #'(lambda (import)
                                       (fallback (gethash class import)
                                                 (ht)))
                                   import-corpora)
            :initial-value (ht)))
  
  (defun imported-file-count (class)
    (reduce #'+ (mapcar #'(lambda (tree)
                            (length (gethash class tree)))
                        import-urls)
            :initial-value 0))

  (defun imported-urls (class)
    (reduce #'append (mapcar #'(lambda (tree)
                                 (gethash class tree))
                             import-urls)
            :initial-value nil))

  (defun recursive-imported-urls (class)
    (reduce #'add-corpuses (append1 (mapcar #'recursive-imported-urls (subclasses class))
                                    (imported-urls class))
            :initial-value (ht)))

  (defun imported-classes ()
    (remove-duplicates (reduce #'append (mapcar #'list-keys import-urls)
                               :initial-value nil)
                       :test #'equal)))

;;; IMPORT/EXPORT
;;;----------------------------------------------------------------------------------------------
;;; TEXT HELPER DATABASES

(defun build-text-database ()
  (dolist (num (list-values (url-aliases)))
    (overwrite-file (concat *text-folder* num) 
                    (extract-text (read-from-file (concat *html-folder* num))))))

(defun build-core-text-database ()
  (ensure-directories-exist *domain-lists-folder*)
  (ensure-directories-exist *boilerplate-folder*)
  (if (not (directory *domain-aliases-file*))
      (overwrite-file *domain-aliases-file* nil))
  (let* ((urls (list-keys (url-aliases)))
         (domains (remove-duplicates (mapcar #'find-domain urls) :test #'equal))
         (domain-lists (map-to-hash #'(lambda (domain) (remove-if-not #'(lambda (url) (equal (find-domain url) domain))
                                                                      urls))
                                    domains)))
    (princ (make-string (length domains) :initial-element #\.))
    (terpri)
    (dolist (domain domains)
      (princ ".")
      (add-domain-alias domain)
      (if (not (equal (read-domain-urls domain)
                      (gethash domain domain-lists)))
          (let ((boilerplate (mapcar #'(lambda (words) (cl-strings:join words :separator " "))
                                     (boilerplate domain))))
            (overwrite-file (concat *domain-lists-folder* (domain-alias domain))
                            (gethash domain domain-lists))
            (overwrite-file (concat *boilerplate-folder* (domain-alias domain))
                            boilerplate)
            (dolist (url (gethash domain domain-lists))
              (let ((num (file-alias url)))
                
                (overwrite-file (concat *core-text-folder* num)
                                (reduce #'remove-substr (append (list (read-text url))
                                                                boilerplate))))))))))

;;; TEXT HELPER DATABASES
;;;----------------------------------------------------------------------------------------------
;;; BUILD CORPUS

(defun get-corpus (class)
  (reduce #'add-corpuses
          (mapcar #'downloaded-url-corpus
                  (class-urls class))
          :initial-value (ht)))

(defun downloaded-url-corpus (url)
  ;; Looks into the downloaded & processed file of the url
  (tokens (read-core-text url)))
