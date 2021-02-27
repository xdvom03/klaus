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
;;; REBUILDING CORPUS

(defun show-time (timer message)
  ;; gets the time of start
  (info-box (concat message " Time taken: " (my-round (/ (- (get-internal-real-time) timer) internal-time-units-per-second))) "success!"))

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
          (multiple-value-bind (subfolder-corpus subfolder-url-count) (rebuild-corpus subclass)
            (incf url-count subfolder-url-count)
            (setf corpus (add-corpuses corpus (scale-corpus subfolder-corpus (get-weight subclass))))))
        
        (multiple-value-bind (class-corpus class-file-count) (get-corpus class)
          (incf url-count class-file-count)
          (setf corpus (add-corpuses corpus class-corpus)))
        (print-to-file "file-count"
                       url-count)
        (print-to-file "word-count"
                       (word-count corpus))
        (print-to-file "corpus" (sort-corpus (hashtable-to-assoc corpus)))
        (print (concat "rebuilt: " class))
        (if (equal class "/")
            (show-time timer "Rebuilt the corpus."))
        (values corpus url-count)))))

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
  ;; Looks into the downloaded & processed file of the link
  (tokens (read-core-text link)))

(defun build-text-database ()
  (let ((timer (get-internal-real-time)))
    (dolist (num (mapcar #'cdr (aliases)))
      (overwrite-direct-file (concat *text-folder* num) 
                             (extract-text (read-from-file (concat *html-folder* num)))))
    (show-time timer "Rebuilt the text database.")))

(defun build-core-text-database ()
  (ensure-directories-exist *domain-lists-folder*)
  (ensure-directories-exist *boilerplate-folder*)
  (if (not (directory *domain-aliases-file*))
      (overwrite-direct-file *domain-aliases-file* nil))
  (let* ((timer (get-internal-real-time))
         (urls (mapcar #'car (aliases)))
         (domains (remove-duplicates (mapcar #'find-domain urls) :test #'equal))
         (domain-lists (map-to-hash #'(lambda (domain) (remove-if-not #'(lambda (url) (equal (find-domain url) domain))
                                                                      urls))
                                    domains)))
    (dolist (domain domains)
      (readd-domain-alias domain)
      (if (not (equal (read-domain-links domain)
                      (gethash domain domain-lists)))
          (let ((boilerplate (mapcar #'(lambda (words) (join words " "))
                                     (boilerplate domain))))
            (overwrite-direct-file (concat *domain-lists-folder* (domain-alias domain))
                                   (gethash domain domain-lists))
            (overwrite-direct-file (concat *boilerplate-folder* (domain-alias domain))
                                   boilerplate)
            (dolist (url (gethash domain domain-lists))
              (let ((num (file-alias url)))
                
                (overwrite-direct-file (concat *core-text-folder* num)
                                       ;; TBD: Core text works less than expected
                                       (reduce #'remove-substr (append (list (read-text url))
                                                                       boilerplate))))))))
    
    (show-time timer "Rebuilt the core text database.")))
