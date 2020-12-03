(defun occurrences (word corpus)
  (let ((corpus-entry (gethash word corpus)))
    (fallback corpus-entry 0)))

(defun wordlist (text)
  ;; TBD: Fix name (hashtable!)
  ;; Produces a corpus (word counts in a hash table) out of a text. Hash table format,  
  (let ((lst (remove-if #'(lambda (word) (equal word ""))
                        (split text #\ )))
        (corpus (make-hash-table :test #'equal)))
    (setf (gethash nil corpus) (remove-duplicates lst :test #'equal))
    (dolist (word lst)
      (if (gethash word corpus)
          (incf (gethash word corpus) 1)
          (setf (gethash word corpus) 1)))
    corpus))

(defun word-count (corpus)
  (let ((vocab (gethash nil corpus))
        (acc 0))
    (dolist (word vocab)
      (incf acc (occurrences word corpus)))
    acc))

(defun normalize-corpus (corp num words)
  ;; Scales the hash table format by a factor of num. Only carries over "words", not the full corpus.
  (let* ((acc (make-hash-table :test #'equal)))
    (dolist (word words)
      (setf (gethash word acc) (* num (occurrences word corp))))
    (setf (gethash nil acc) words)
    acc))

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
      
      (let* ((data (get-corpus folder)))
        (incf url-count (car data))
        (setf corpus (add-hashtable-corpuses corpus (cdr data))))
      ;; TEMP: Changed for word, not document count.
      (print-to-file "file-count" ;; url-count
                     (word-count corpus))
      (print-to-file "corpus" (corpus-list corpus))
      (if (equal folder *classes-folder*)
          (log-print "Rebuilt the corpus. Time taken: "
                     ;; internal-time-units-per-second is a LISP built-in constant
                     (my-round (/ (- (get-internal-real-time) timer) internal-time-units-per-second))))
      (cons url-count corpus))))

(defun add-hashtable-corpuses (corp1 corp2)
  ;; Works with the hashtable format
  (let ((acc (make-hash-table :test #'equal))
        (wordlist (remove-duplicates (append (gethash nil corp1)
                                             (gethash nil corp2))
                                     :test #'equal)))
    (dolist (word wordlist)
      (setf (gethash word acc)
            (+ (occurrences word corp1)
               (occurrences word corp2))))
    (setf (gethash nil acc) wordlist)
    acc))

(defun sort-corpus (corp)
  ;; Only applicable for the list format of corpuses.
  (sort (copy-seq corp) #'< :key #'cdr))

(defun get-corpus (folder)
  ;; Returns cons of url count & corpus
  (let ((vocab-lists (mapcar #'wordlist
                             (mapcar #'(lambda (file-name) (extract-text (file-content file-name)))
                                     (class-links folder)))))
    (cons (length vocab-lists)
          (if vocab-lists
              (reduce #'add-hashtable-corpuses
                      vocab-lists)
              ;; Create an empty hash table
              (make-hash-table :test #'equal)))))

(defun corpus-hashtable (list)
  ;; Converts from the list format to the hash table format
  ;; NIL contains a list of all hashes to be used when adding or subtracting.
  (let ((corpus (make-hash-table :test #'equal)))
    (dolist (word list)
      (setf (gethash (car word) corpus) (cdr word)))
    (setf (gethash nil corpus) (mapcar #'car list))
    corpus))

(defun corpus-list (hashtable)
  ;; Converts from the hash table format to the list format
  (let ((corpus nil))
    (dolist (word (gethash nil hashtable))
      (push (cons word (occurrences word hashtable)) corpus))
    (sort-corpus corpus)))

(defun get-recursive-corpus (folder)
  ;; Looks into the corpus file and converts to the hash-table formulation
  (corpus-hashtable (read-from-file (concat folder "corpus"))))

(defun get-subfolder-corpus (folder)
  ;; Like get-recursive-corpus, but without the files in the folder itself.
  (corpus-subtract (get-recursive-corpus folder)
                   (cdr (get-corpus folder))))

(defun corpus-subtract (corp1 corp2)
  ;; Works with hash tables
  ;; Assumes that corp2 is a subcorpus of corp2, so there is nothing negative in the result (and the vocabulary of corp2 is included in the vocabulary of corp1)
  ;; No need to remove words with 0 occurrences. TBD: In case of bottleneck, check if it makes the program faster.
  (let ((acc (make-hash-table :test #'equal)))
    (dolist (word (gethash nil corp1))
      (setf (gethash word acc)
            (- (occurrences word corp1)
               (occurrences word corp2))))
    (setf (gethash nil acc) (gethash nil corp1))
    acc))

(defun get-file-count (folder)
  ;; Just looks into the file-count file
  (read-from-file (concat folder "file-count")))
