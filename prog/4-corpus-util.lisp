(defun occurrences (word corpus)
  (let ((corpus-entry (gethash word corpus)))
    (fallback corpus-entry 0)))

(defun text-corpus (text)
  (let* ((tokens (tokens text))
         (corpus (make-hash-table :test #'equal))
         ;; TEMP: Use this for document weights
         (weight 1))
    (dolist (token tokens)
      (if (gethash token corpus)
          (incf (gethash token corpus) weight)
          (setf (gethash token corpus) weight)))
    corpus))

(defun word-count (corpus)
  (let ((vocab (list-keys corpus))
        (acc 0))
    (dolist (word vocab)
      (incf acc (occurrences word corpus)))
    acc))

(defun scale-corpus (corp num)
  (map-to-hash #'(lambda (word)
                   (* num (occurrences word corp)))
               (mapcar #'car (list-hashes corp))))

(defun rebuild-corpus (&optional (folder *classes-folder*))
  ;; Writes the cons list format of the corpuses into the respective files.
  (let ((corpus-folder (generated-folder folder)))
    (labels ((print-to-file (file-name &rest things)
               (apply #'overwrite-file (concat corpus-folder file-name) things)))
    
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
        (print (concat "Rebuilt the corpus. Folder: "
                       folder
                       ;; internal-time-units-per-second is a LISP built-in constant
                       " Time taken: "
                       (my-round (/ (- (get-internal-real-time) timer) internal-time-units-per-second))))
        (cons url-count corpus)))))

(defun add-hashtable-corpuses (corp1 corp2)
  (map-to-hash #'(lambda (word) (if (>= (+ (occurrences word corp1)
                                           (occurrences word corp2))
                                        0)
                                    (+ (occurrences word corp1)
                                       (occurrences word corp2))
                                    (progn (print word)
                                           0)))
               (remove-duplicates (append (list-keys corp1)
                                          (list-keys corp2))
                                  :test #'equal)))

(defun sort-corpus (corp)
  ;; Only applicable to the list format of corpuses.
  (sort (copy-seq corp) #'< :key #'cdr))

(defun downloaded-link-corpus (link)
  ;; Looks into the downloaded file of the link
  (text-corpus (extract-text (file-content link))))

(defun get-corpus (folder)
  ;; Returns cons of url count & corpus
  (let ((vocab-lists (mapcar #'downloaded-link-corpus
                             (class-links folder))))
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

(defun generated-folder (class)
  ;; Returns the path to where generated data for that class is stored
  (concat *generated-data-folder* (subseq (simplified-path class) 1)))

(defun get-generated-data (class data-name)
  (read-from-file (concat (generated-folder class) data-name)))

(defun get-recursive-corpus (class)
  ;; Looks into the corpus file and converts to the hash-table formulation
  ;; The vast majority of time is spent here, loading the corpus files.
  (corpus-hashtable (get-generated-data class "corpus")))

(defun get-file-count (class)
  (fallback (get-generated-data class "file-count") 0))

(defun get-word-count (class)
  (fallback (get-generated-data class "word-count") 0))
