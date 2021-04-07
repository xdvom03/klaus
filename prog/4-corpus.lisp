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
;;; BOILERPLATE

(defun overlap-length (txt1 txt2)
  ;; in txt1, only looks for sequences beginning at 0
  (let ((max-len (min (length txt1)
                      (length txt2))))
    (do ((len 0 (1+ len)))
        ((or (equal len max-len)
             (not (search (subseq txt1 0 (1+ len)) txt2 :test #'equal)))
         len))))

(defun overlap (txt1 txt2)
  (let ((len (min (length txt1) (length txt2)))
        (acc nil))
    (do ((pos 0))
        ((>= pos (- len *boilerplate-threshold*)))
      (if (search (subseq txt1 pos (+ pos *boilerplate-threshold*))
                  txt2
                  :test #'equal)
          (let ((overlap (overlap-length (subseq txt1 pos) txt2)))
            (push (subseq txt1 pos (+ pos overlap)) acc)
            (incf pos overlap))
          (incf pos)))
    (reverse acc)))

(defun multi-overlap (texts txt2)
  (apply #'append
         (mapcar #'(lambda (txt1)
                     (overlap txt1 txt2))
                 texts)))

(defun boilerplate (domain)
  ;; Works with words as strings, since this is still operating at the string level, to make the core string
  (let ((urls (remove-if-not #'(lambda (url)
                                 (equal (find-domain url)
                                        domain))
                             (class-urls "/" t))))
    (if (cdr urls)
        (reduce #'multi-overlap
                (append (list (list (cl-strings:split (read-text (car urls))))) ;; remember: one level of append gets lost!
                        (mapcar #'(lambda (url) (cl-strings:split (read-text url)))
                                (cdr urls)))))))

(defun remove-substr (text substr)
  ;; TEMP: Obviously not optimal, should not happen! Probably an underlying bug.
  (fallback (ignore-errors
             (let ((index (search substr text :test #'equal)))
               (concat (subseq text 0 index) (subseq text (+ index (length substr))))))
            text))

;;; BOILERPLATE
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
  (ltk:with-ltk ()
    (ltk:wm-title ltk:*tk* "Building core text...")
    (let* ((pb (progress-bar 0 0 ltk:*tk*))
           (urls (list-keys (url-aliases)))
           (domains (remove-duplicates (mapcar #'find-domain urls) :test #'equal))
           (domain-lists (map-to-hash #'(lambda (domain) (remove-if-not #'(lambda (url) (equal (find-domain url) domain))
                                                                        urls))
                                      domains))
           (total-files (length urls))
           (counter 0))
      (labels ((progress (&optional (count 1))
                 (setf (ltk-mw:percent pb)
                       (* 100 (/ (incf counter count)
                                 total-files))))
               (build-domain-core-text (domain)
                 ;; Manual files have domain NIL and are exempt from this
                 (if (equal (read-domain-urls domain)
                            (gethash domain domain-lists))
                     (progress (length (gethash domain domain-lists)))
                     (if domain
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
                                                                               boilerplate)))
                               (progress))))
                         (dolist (url (gethash domain domain-lists))
                           (let ((num (file-alias url)))
                             (overwrite-file (concat *core-text-folder* num)
                                             (read-text url)))
                           (progress))))))
        (dolist (domain domains)
          (add-domain-alias domain)
          (build-domain-core-text domain)))
      (ltk:destroy ltk:*tk*))))

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
