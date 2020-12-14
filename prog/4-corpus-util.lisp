(defun occurrences (word corpus)
  (let ((corpus-entry (gethash word corpus)))
    (fallback corpus-entry 0)))

(defun text-corpus (text)
  (let ((tokens (tokens text))
        (corpus (make-hash-table :test #'equal)))
    (dolist (token tokens)
      (if (gethash token corpus)
          (incf (gethash token corpus) 1)
          (setf (gethash token corpus) 1)))
    corpus))

(defun word-count (corpus)
  (let ((vocab (list-keys corpus))
        (acc 0))
    (dolist (word vocab)
      (incf acc (occurrences word corpus)))
    acc))

(defun normalize-corpus (corp num words)
  ;; Scales the hash table format by a factor of num. Only carries over "words", not the full corpus.
  (map-to-hash #'(lambda (word)
                   (* num (occurrences word corp)))
               words))

(defun rebuild-corpus (&optional (folder *classes-folder*))
  ;; Writes the cons list format of the corpuses into the respective files.
  (labels ((print-to-file (file-name &rest things)
             (apply #'overwrite-file (concat folder file-name) things)))
    
    (let ((subfolders (subfolders folder))
          (url-count 0)
          (corpus (make-hash-table :test #'equal))
          (timer (get-internal-real-time)))
      
      (dolist (subfolder subfolders)
        (let ((data (rebuild-corpus subfolder)))
          (incf url-count (car data))
          (setf corpus (add-hashtable-corpuses corpus (cdr data)))))
      
      (multiple-value-bind (folder-corpus folder-file-count) (get-corpus folder)
        (incf url-count folder-file-count)
        (setf corpus (add-hashtable-corpuses corpus folder-corpus)))
      (print-to-file "file-count"
                     url-count)
      (print-to-file "word-count"
                     (word-count corpus))
      (print-to-file "corpus" (corpus-list corpus))
      (if (equal folder *classes-folder*)
          (log-print "Rebuilt the corpus. Time taken: "
                     ;; internal-time-units-per-second is a LISP built-in constant
                     (my-round (/ (- (get-internal-real-time) timer) internal-time-units-per-second))))
      (cons url-count corpus))))

(defun add-hashtable-corpuses (corp1 corp2)
  (map-to-hash #'(lambda (word) (+ (occurrences word corp1)
                                   (occurrences word corp2)))
               (remove-duplicates (append (list-keys corp1)
                                          (list-keys corp2))
                                  :test #'equal)))

(defun sort-corpus (corp)
  ;; Only applicable to the list format of corpuses.
  (sort (copy-seq corp) #'< :key #'cdr))

(defun get-corpus (folder)
  ;; Returns cons of url count & corpus
  (let ((vocab-lists (mapcar #'text-corpus
                             (mapcar #'(lambda (file-name) (extract-text (file-content file-name)))
                                     (class-links folder)))))
    (values (if vocab-lists
                (reduce #'add-hashtable-corpuses
                        vocab-lists)
                ;; Create an empty hash table
                (make-hash-table :test #'equal))
            (length vocab-lists))))

(defun corpus-hashtable (list)
  ;; Converts from the list format to the hash table format
  (map-to-hash #'cdr list :key-fun #'car))

(defun corpus-list (hashtable)
  ;; Converts from the hash table format to the list format
  (let ((corpus nil))
    (dolist (word (list-keys hashtable))
      (push (cons word (occurrences word hashtable)) corpus))
    (sort-corpus corpus)))

(defun get-recursive-corpus (folder)
  ;; Looks into the corpus file and converts to the hash-table formulation
  ;; The vast majority of time is spent here, loading the corpus files.
  (corpus-hashtable (read-from-file (concat folder "corpus"))))

(defun get-file-count (folder)
  ;; Just looks into the file-count file
  (read-from-file (concat folder "file-count")))

(defun get-word-count (folder)
  ;; Just looks into the word-count file
  (read-from-file (concat folder "word-count")))
