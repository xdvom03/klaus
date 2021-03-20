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

(defun export-corpora ()
  (hashtable-to-assoc (let ((hash (make-hash-table :test #'equal)))
                        (apply-to-all-classes #'(lambda (class)
                                                  (setf (gethash class hash)
                                                        (sort-corpus (hashtable-to-assoc (get-corpus class))))))
                        hash)))

(defun export-url-tree ()
  (let ((hash (make-hash-table :test #'equal)))
    (apply-to-all-classes #'(lambda (class)
                              (setf (gethash class hash)
                                    (class-urls class))))
    (hashtable-to-assoc hash)))

(defun create-export (path)
  (append-to-file path (export-url-tree))
  (append-to-file path (export-corpora)))

(defun imports ()
  (directory (concat *imports-folder* "*")))

(defun create-classes-in-corpus (corp)
  (dolist (class (list-keys corp))
    (if (recursive-create-class class)
        (print (concat "Created: " class)))))

(defun create-all-import-classes ()
  (dolist (corp-file (imports))
    (create-classes-in-corpus (assoc-to-hashtable (read-from-file corp-file)))))

(let (url-trees
      assoc-import-corpora
      import-corpora
      import-urls)
  (defun refresh-imports ()
    (let ((data (mapcar #'(lambda (import-file)
                            (with-open-file (stream import-file :direction :input :if-does-not-exist :error)
                              (cons (read stream)
                                    (read stream))))
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
                                                 (make-hash-table :test #'equal)))
                                   import-corpora)
            :initial-value (make-hash-table :test #'equal)))
  
  (defun imported-file-count (class)
    (reduce #'+ (mapcar #'(lambda (tree)
                            (length (fallback (gethash class tree)
                                              nil)))
                        import-urls)
            :initial-value 0)))

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
  (let* ((urls (class-urls "/" t))
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
;;; CORPUS BUILDING

(defun rebuild-corpus (&optional (class "/"))
  (if (equal class "/")
      (refresh-imports))
  ;; Writes the cons list format of the corpuses into the respective files. Writes corpus metadata.
  (let ((corpus-folder (generated-folder class)))
    (labels ((print-to-file (file-name &rest things)
               (apply #'overwrite-file (concat corpus-folder file-name) things)))
      
      (let ((subclasses (subclasses class))
            (url-count 0)
            (corpus (make-hash-table :test #'equal)))
        
        (dolist (subclass subclasses)
          (multiple-value-bind (subfolder-corpus subfolder-url-count) (rebuild-corpus subclass)
            (incf url-count subfolder-url-count)
            (setf corpus (add-corpuses corpus (scale-corpus subfolder-corpus (get-weight subclass))))))
        
        (multiple-value-bind (class-corpus class-file-count) (get-corpus class)
          (incf url-count (+ class-file-count
                             (imported-file-count class)))
          (setf corpus (add-corpuses (add-corpuses corpus class-corpus)
                                     (imported-corpus class))))
        (print-to-file "file-count"
                       url-count)
        (print-to-file "word-count"
                       (word-count corpus))
        (print-to-file "corpus" (sort-corpus (hashtable-to-assoc corpus)))
        (if (equal class "/")
            (refresh-corpora))
        (print (concat "rebuilt: " class))
        (values corpus url-count)))))

(defun get-corpus (class)
  ;; Returns cons of url count & corpus
  (let ((vocab-lists (mapcar #'downloaded-url-corpus
                             (class-urls class))))
    (values (if vocab-lists
                (reduce #'add-corpuses
                        vocab-lists)
                ;; Create an empty hash table
                (make-hash-table :test #'equal))
            (length vocab-lists))))

(defun generated-folder (class)
  ;; Returns the path to where generated data for that class is stored (must remove starting slash, thus the subseq)
  (concat *generated-data-folder* (subseq class 1)))

(defun get-generated-data (class data-name)
  (read-from-file (concat (generated-folder class) data-name)))

(labels ((read-corpora ()
           (map-to-hash #'(lambda (class)
                            (assoc-to-hashtable (get-generated-data class "corpus")))
                        (let ((acc nil))
                          (apply-to-all-classes #'(lambda (cl) (push cl acc)))
                          acc))))
  (let ((corpora (read-corpora)))
    (defun get-recursive-corpus (class)
      (gethash class corpora))
    
    (defun refresh-corpora ()
      (refresh-imports)
      (setf corpora (read-corpora)))))

(defun get-file-count (class)
  (fallback (get-generated-data class "file-count") 0))

(defun get-word-count (class)
  (fallback (floor (get-generated-data class "word-count")) 0))

(defun downloaded-url-corpus (url)
  ;; Looks into the downloaded & processed file of the url
  (tokens (read-core-text url)))

;;; CORPUS BUILDING
;;;----------------------------------------------------------------------------------------------
;;; CRAWLER RESULTS PROCESSING

(defun build-hollow-corpus (&optional (class "/"))
  ;; corpus to be used only for displaying files
  (let ((corpus-folder (generated-folder class)))
    (labels ((print-to-file (file-name &rest things)
               (apply #'overwrite-file (concat corpus-folder file-name) things)))
      (let ((subclasses (subclasses class))
            (url-count 0))
        (dolist (subclass subclasses)
          (incf url-count (build-hollow-corpus subclass)))
        (incf url-count (length (class-urls class)))
        (print-to-file "file-count" url-count)
        (print-to-file "word-count" 0)
        (print-to-file "corpus" nil)
        url-count))))
