(defun pagerank (options scores-table)
  ;; Gets logarithmic formulation
  ;; Pageranking one or zero classes results in division 0/0
  (labels ((option-raw-score (option)
             (if (< (length options) 2)
                 1
                 (apply #'+ (mapcar #'(lambda (option2)
                                        (if (equal option option2)
                                            0
                                            (gethash (cons option option2) scores-table)))
                                    options)))))
    (let* ((raw-scores (mapcar #'option-raw-score
                               options))
           (probsum (apply #'ln+ raw-scores)))
      (values (map-to-hash (compose #'exp
                                    #'(lambda (score) (- score probsum))
                                    #'option-raw-score)
                           options)
              ;; For testing purposes
              probsum))))

(defun smooth-ratio (target total subclass-count smoothing-factor)
  (- (ln (+ smoothing-factor target))
     (ln (+ (* smoothing-factor subclass-count) total))))

(defun word-probability (word target total subclass-count)
  (smooth-ratio (occurrences word target)
                (occurrences word total)
                subclass-count
                *smoothing-factor*))

;;; SCORE MATH
;;;----------------------------------------------------------------------------------------------
;;; FROM MAIN

(defun chosen-words (vocab word-scores)
  (let* ((ordered-words (sort (copy-seq vocab)
                              #'<
                              :key #'(lambda (word) (gethash word word-scores))))
         (evidence-length (max *evidence-length*
                               (floor (/ (length (remove-if #'(lambda (word) (>= (ln *max-word-score*)
                                                                                 (gethash word word-scores)
                                                                                 (ln *min-word-score*)))
                                                            ordered-words))
                                         2)))))
    (subseq ordered-words 0 (min evidence-length
                                 (length ordered-words)))))

(defun compare-classes (vocab paths corpuses &optional (want-evidence? t))
  ;; Returns a cons of two hash tables. A hash table of path -> score, and a hash table of path -> chosen words.
  ;; Already gets normalised corpuses
  ;; TBD: Make this two-class. It's ridiculous.
  (let* ((class-count (length (list-keys corpuses)))
         (scores (make-hash-table :test #'equal))
         (evidence-words (make-hash-table :test #'equal))
         (evidence-details (make-hash-table :test #'equal)))
    (dolist (path paths)
      ;; balance evidence for and against
      (let* ((corpus (gethash path corpuses))
             ;; we need a score for every word-path pair
             (word-scores (map-to-hash #'(lambda (word)
                                           (smooth-ratio (occurrences word corpus)
                                                         (apply #'+ (mapcar #'(lambda (path) (occurrences word (gethash path corpuses)))
                                                                            paths))
                                                         class-count
                                                         *smoothing-factor*))
                                       vocab))
             (chosen-words (chosen-words vocab word-scores))
             (score (apply #'+ (mapcar #'(lambda (word) (gethash word word-scores))
                                       chosen-words))))
        (setf (gethash path scores) score)
        (if want-evidence?
            (progn
              (setf (gethash path evidence-words) chosen-words)
              (setf (gethash path evidence-details) (map-to-hash #'(lambda (word)
                                                                     (concat (apply #'concat (mapcar #'(lambda (path) (concat " / " (my-round (occurrences word (gethash path corpuses))))) paths))
                                                                             " -> "
                                                                             (my-round (exp (gethash word word-scores)))))
                                                                 vocab))))))

    (let ((prob-sum (apply #'ln+ (mapcar #'(lambda (path) (gethash path scores)) paths))))
      (dolist (path paths)
        (setf (gethash path scores)
              (- (gethash path scores)
                 prob-sum))))
    (values scores evidence-words evidence-details)))

(defun scores (vocab classes corpuses word-counts &optional (want-evidence? t))
  ;; excluded data is a cons of (url . class) used for blind checks
  ;; In classes without subclasses, we don't want to do anything
  (if classes
      (let* ((pair-scores (make-hash-table :test #'equal))
             (pair-words (make-hash-table :test #'equal))
             (pair-word-details (make-hash-table :test #'equal)))
        (dolist (class classes)
          (dolist (opponent (cdr (member class classes :test #'equal))) ; only check classes coming after it
            (multiple-value-bind (scores evidence-words evidence-details) (let ((min-size (min (gethash class word-counts)
                                                                                               (gethash opponent word-counts))))
                                                                            (compare-classes vocab
                                                                                             (list class opponent)
                                                                                             (map-to-hash #'(lambda (path)
                                                                                                              (scale-corpus (gethash path corpuses)
                                                                                                                            (/ min-size (gethash path word-counts))))
                                                                                                          (list class opponent))
                                                                                             want-evidence?))
              (setf (gethash (cons class opponent) pair-scores)
                    (gethash class scores))
              (setf (gethash (cons opponent class) pair-scores)
                    (gethash opponent scores))
              (if want-evidence?
                  (progn
                    (setf (gethash (cons class opponent) pair-words)
                          (gethash class evidence-words))
                    (setf (gethash (cons class opponent) pair-word-details)
                          (gethash class evidence-details))
                    (setf (gethash (cons opponent class) pair-words)
                          (gethash opponent evidence-words))
                    (setf (gethash (cons opponent class) pair-word-details)
                          (gethash opponent evidence-details)))))))
        ;; the actual scores and some data for the explainer
        (multiple-value-bind (scores probsum) (pagerank classes pair-scores)
          (values scores
                  probsum
                  pair-scores
                  pair-words
                  pair-word-details)))
      (values (make-hash-table :test #'equal)
              1)))
