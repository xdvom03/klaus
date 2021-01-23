(defun occurrences (word corpus)
  (fallback (gethash word corpus) 0))

(defun text-corpus (text)
  (let* ((tokens (tokens text))
         (corpus (make-hash-table :test #'eq))
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

(defun add-corpuses (corp1 corp2)
  (map-to-hash #'(lambda (word) (+ (occurrences word corp1)
                                   (occurrences word corp2)))
               (remove-duplicates (append (list-keys corp1)
                                          (list-keys corp2))
                                  :test #'equal)))

(defun sort-corpus (corp)
  ;; Only applicable to the list format of corpuses.
  (sort (copy-seq corp) #'< :key #'cdr))

;;; CORPUS UTIL
;;;----------------------------------------------------------------------------------------------
;;; FOLDER NAVIGATION 

(defun rebuild-corpus (&optional (class "/"))
  ;; Writes the cons list format of the corpuses into the respective files.
  (let ((corpus-folder (generated-folder class)))
    (labels ((print-to-file (file-name &rest things)
               (apply #'overwrite-direct-file (concat corpus-folder file-name) things)))
    
      (let ((subclasses (subclasses class))
            (url-count 0)
            (corpus (make-hash-table :test #'equal))
            (timer (get-internal-real-time)))
      
        (dolist (subclass subclasses)
          ;; TBD: Use values!
          (let ((data (rebuild-corpus subclass)))
            (incf url-count (car data))
            (setf corpus (add-corpuses corpus (cdr data)))))
      
        (multiple-value-bind (class-corpus class-file-count) (get-corpus class)
          (incf url-count class-file-count)
          (setf corpus (add-corpuses corpus class-corpus)))
        (print-to-file "file-count"
                       url-count)
        (print-to-file "word-count"
                       (word-count corpus))
        (print-to-file "corpus" (sort-corpus (hashtable-to-assoc corpus)))
        ;; internal-time-units-per-second is a LISP built-in constant
        (if (equal class "/")
            (info-box (concat "Rebuilt the corpus. Time taken: " (my-round (/ (- (get-internal-real-time) timer) internal-time-units-per-second))) "success!"))
        (cons url-count corpus)))))

(defun get-corpus (class)
  ;; Returns cons of url count & corpus
  (let ((vocab-lists (mapcar #'downloaded-link-corpus
                             (class-links class))))
    (values (if vocab-lists
                (reduce #'add-corpuses
                        vocab-lists)
                ;; Create an empty hash table
                (make-hash-table :test #'equal))
            (length vocab-lists))))

(defun generated-folder (class)
  ;; Returns the path to where generated data for that class is stored
  (concat *generated-data-folder* (subseq class 1)))

(defun get-generated-data (class data-name)
  (read-from-file (concat (generated-folder class) data-name)))

(defun get-recursive-corpus (class)
  ;; Looks into the corpus file and converts to the hash-table formulation
  ;; The vast majority of time is spent here, loading the corpus files.
  (assoc-to-hashtable (get-generated-data class "corpus")))

(defun get-file-count (class)
  (fallback (get-generated-data class "file-count") 0))

(defun get-word-count (class)
  (fallback (get-generated-data class "word-count") 0))

(defun downloaded-link-corpus (link)
  ;; Looks into the downloaded file of the link
  (text-corpus (extract-text (file-content link))))
