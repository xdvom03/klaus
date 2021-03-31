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

(let (import-corpora
      import-urls)
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
