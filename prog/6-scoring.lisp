(defun pagerank (options scores-table)
  ;; Gets logarithmic formulation
  ;; Pageranking one or zero folders results in division 0/0
  (if (< (length options) 2)
      (let ((scores (make-hash-table :test #'equal)))
        (dolist (option options)
          (setf (gethash option scores) 1))
        scores)
      (let ((scores (make-hash-table :test #'equal))
            (acc (make-hash-table :test #'equal))
            (final-probsum 0))
        (dolist (option options)
          (setf (gethash option scores)
                (ln (/ (length options)))))
        (do ((i *iterations* (1- i)))
            ((or (zerop i)
                 (some #'(lambda (option)
                           (> (gethash option scores) -0.001))
                       options)))
          (dolist (option options)
            (setf (gethash option acc)
                  #|(- (apply #'ln+
                            (remove-if #'null
                                       (mapcar #'(lambda (option2)
                                                   (if (not (equal option option2))
                                                       (+ (gethash option2 scores)
                                                          (gethash (cons option option2) scores-table))))
                                               options)))
                     (apply #'ln+ (remove-if #'null
                                             (mapcar #'(lambda (option2) (if (not (equal option2 option))
                                                                             (gethash option2 scores)))
                                                     options))))|#
                  ;; TBD: Try the geometric average
                  (/ (apply #'+
                            (remove-if #'null
                                       (mapcar #'(lambda (option2)
                                                   (if (not (equal option option2))
                                                       (* (exp (gethash option2 scores))
                                                          (gethash (cons option option2) scores-table))))
                                               options)))
                     (apply #'+ (remove-if #'null
                                           (mapcar #'(lambda (option2)
                                                       (if (not (equal option2 option))
                                                           (exp (gethash option2 scores))))
                                                   options))))))
          (let ((probsum (apply #'ln+ (mapcar #'(lambda (opt) (gethash opt acc))
                                              options))))
            (dolist (option options)
              (setf (gethash option scores)
                    (- (gethash option acc)
                       probsum)))
            (setf final-probsum probsum)))
        (dolist (option options)
          (setf (gethash option scores)
                (exp (gethash option scores))))
        (values scores
                final-probsum))))

(defun smooth-ratio (target total subfolder-count smoothing-factor)
  (- (ln (+ smoothing-factor target))
     (ln (+ (* smoothing-factor subfolder-count) total))))

(defun word-probability (word target total subfolder-count)
  (smooth-ratio (occurrences word target)
                (occurrences word total)
                subfolder-count
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
    (append (subseq ordered-words 0 (min evidence-length
                                         (length ordered-words)))
            (subseq ordered-words (max 0
    (- (length ordered-words) evidence-length))))))

(defun compare-folders (vocab paths corpuses)
  ;; Returns a cons of two hash tables. A hash table of path -> score, and a hash table of path -> chosen words.
  ;; Already gets normalised corpuses
  (let* ((folder-count (length (list-keys corpuses)))
         (total-corpus (reduce #'add-hashtable-corpuses
                               (mapcar #'(lambda (path) (gethash path corpuses))
                                       paths)))
         (scores (make-hash-table :test #'equal))
         (evidence-words (make-hash-table :test #'equal))
         (evidence-scores (make-hash-table :test #'equal)))
    (dolist (path paths)
      ;; balance evidence for and against
      (let* ((corpus (gethash path corpuses))
             ;; we need a score for every word-path pair
             (word-scores (let ((acc (make-hash-table :test #'equal)))
                            (dolist (word vocab)
                              (setf (gethash word acc)
                                    (word-probability word corpus total-corpus folder-count)))
                            acc))
             (chosen-words (chosen-words vocab word-scores))
             (score (apply #'+ (mapcar #'(lambda (word) (gethash word word-scores))
                                       chosen-words))))
        (setf (gethash path scores) score)
        (setf (gethash path evidence-words) chosen-words)
        (setf (gethash path evidence-scores) word-scores)))

    (let ((prob-sum (apply #'ln+ (mapcar #'(lambda (path) (gethash path scores)) paths))))
      (dolist (path paths)
        (setf (gethash path scores)
              (- (gethash path scores)
                 prob-sum))))
    (values scores evidence-words evidence-scores)))

(defun scores (vocab folders corpuses word-counts)
  ;; In folders without subfolders, we don't want to do anything
  (if (and folders *try-to-class?*)
      (let ((pair-scores (make-hash-table :test #'equal))
            (pair-words (make-hash-table :test #'equal))
            (pair-word-scores (make-hash-table :test #'equal)))
        (dolist (folder folders)
          (dolist (opponent folders)
            (if (equal folder opponent)
                (setf (gethash (cons folder opponent) pair-scores) 0) ; chosen words can remain empty
                (multiple-value-bind (scores evidence-words evidence-scores) (let ((min-size (min (gethash folder word-counts)
                                                                                                  (gethash opponent word-counts))))
                                                                               (compare-folders vocab
                                                                                                (list folder opponent)
                                                                                                (map-to-hash #'(lambda (path)
                                                                                                                 (normalize-corpus (gethash path corpuses)
                                                                                                                                   (/ min-size (gethash path word-counts))
                                                                                                                                   vocab))
                                                                                                             (list folder opponent)))) 
                  (setf (gethash (cons folder opponent) pair-scores)
                        (gethash folder scores))
                  (setf (gethash (cons folder opponent) pair-words)
                        (gethash folder evidence-words))
                  (setf (gethash (cons folder opponent) pair-word-scores)
                        (gethash folder evidence-scores))))))
        ;; the actual scores and some data for the explainer
        (multiple-value-bind (scores probsum) (pagerank folders pair-scores)
          (values scores
                  probsum
                  pair-scores
                  pair-words
                  pair-word-scores)))
      (make-hash-table :test #'equal)))
