(defun pagerank (options scores-table)
  ;; Pageranking one or zero folders results in division 0/0
  (if (< (length options) 2)
      (let ((scores (make-hash-table :test #'equal)))
        (dolist (option options)
          (setf (gethash option scores) 1))
        scores)
      (let ((scores (make-hash-table :test #'equal))
            (acc (make-hash-table :test #'equal))
            (final-probsum nil))
        (dolist (option options)
          (setf (gethash option scores)
                ;; BEWARE of ratios entering this calculation, for they explode in precision, size, and lag.
                (coerce (/ (length options)) 'double-float)))
        (dotimes (i *iterations*)
          (dolist (option options)
            ;; BUG: Possible division by 0. Limit probabilities to double float range. Fix properly with logarithm formulation.
            (setf (gethash option acc)
                  (/ (apply #'+
                            (mapcar #'(lambda (option2)
                                        (if (equal option option2)
                                            0
                                            (* (gethash option2 scores)
                                               (gethash (cons option option2) scores-table))))
                                    options))
                     (apply #'+ (mapcar #'(lambda (option2) (if (equal option2 option)
                                                                (/ 10000000000000000) ; prevents division by zero if it is certain the document belongs in a given category. TBD: Fix properly with logarithms.
                                                                (gethash option2 scores)))
                                        options)))))
          (let ((probsum (apply #'+ (mapcar #'(lambda (opt) (gethash opt acc))
                                            options))))
            ;; BUG: Float precision can fail, causing a 0/0 to appear here. I put it off with double floats, but it needs addressing eventually.
            ;; TBD: Maybe somehow display the eventual probsum, because it is also a result of the calculation, it is unique, and seems to somehow reflect on the certainty of the result
            ;; Probsum: Min 1, Max N/2 for N options
            (dolist (option options)
              (setf (gethash option scores)
                    (/ (gethash option acc)
                       probsum)))
            (setf final-probsum probsum)))
        (values scores
                final-probsum))))

(defun word-probability (target total subfolder-count smoothing-factor)
  (/ (+ smoothing-factor target)
     (+ (* smoothing-factor subfolder-count) total)))

;;; SCORE MATH
;;;----------------------------------------------------------------------------------------------
;;; FROM MAIN

(defun chosen-words (vocab word-scores)
  (let* ((ordered-words (sort (copy-seq vocab)
                              #'<
                              :key #'(lambda (word) (gethash word word-scores))))
         (evidence-length (max *evidence-length*
                               (floor (/ (length (remove-if #'(lambda (word) (>= 4/5 (gethash word word-scores) 1/5))
                                                            ordered-words))
                                         2)))))
    (append (subseq ordered-words 0 (min evidence-length
                                         (length ordered-words)))
            (subseq ordered-words (max 0
                                       (- (length ordered-words) evidence-length))))))

(defun compare-folders (vocab folders)
  ;; Returns a cons of two hash tables. A hash table of path -> score, and a hash table of path -> chosen words.
  (let* ((folder-count (length folders))
         (smallest-folder (apply #'min (mapcar #'get-word-count folders)))
         (corpuses (let ((acc (make-hash-table :test #'equal)))
                     (dolist (path folders)
                       (setf (gethash path acc)
                             (normalize-corpus (get-recursive-corpus path)
                                               (/ smallest-folder (get-word-count path)) ; TBD: Warn if a folder is empty!
                                               vocab)))
                     acc))
         (total-corpus (reduce #'add-hashtable-corpuses
                               (mapcar #'(lambda (path) (gethash path corpuses))
                                       folders)))
         (scores (make-hash-table :test #'equal))
         (evidence (make-hash-table :test #'equal)))
    (dolist (path folders)
      ;; balance evidence for and against
      (let* ((corpus (gethash path corpuses))
             ;; we need a score for every word-path pair
             (word-scores (let ((acc (make-hash-table :test #'equal)))
                            (dolist (word vocab)
                              (setf (gethash word acc)
                                    (word-probability (occurrences word corpus)
                                                      (occurrences word total-corpus)
                                                      folder-count
                                                      *smoothing-factor*)))
                            acc))
             (chosen-words (chosen-words vocab word-scores))
             (score (apply #'* (mapcar #'(lambda (word) (gethash word word-scores))
                                       ;; Need the best words here
                                       chosen-words))))
        (setf (gethash path scores) score)
        (setf (gethash path evidence) chosen-words)))

    ;; Potential BUG: Very long ratios
    (let ((prob-sum (apply #'+ (mapcar #'(lambda (path) (gethash path scores)) folders))))
      (dolist (path folders)
        (setf (gethash path scores)
              (coerce (/ (gethash path scores)
                         prob-sum)
                      'double-float))))
    (cons scores evidence)))

(defun scores (vocab folders)
  ;; In folders without subfolders, we don't want to do anything
  (if (and folders *try-to-class?*)
      (let ((pair-scores (make-hash-table :test #'equal))
            (pair-chosen-words (make-hash-table :test #'equal)))
        (dolist (folder folders)
          (dolist (opponent folders)
            (if (equal folder opponent)
                (setf (gethash (cons folder opponent) pair-scores) 0) ; chosen words can remain empty
                ;; TBD: multiple-value-bind?
                (let* ((data (compare-folders vocab (list folder opponent)))
                       (scores (car data))
                       (evidence (cdr data)))
                  (setf (gethash (cons folder opponent) pair-scores)
                        (gethash folder scores))
                  (setf (gethash (cons folder opponent) pair-chosen-words)
                        (gethash folder evidence))))))
        ;; the actual scores and some data for the explainer
        (multiple-value-bind (scores probsum) (pagerank folders pair-scores)
          (values scores
                  probsum
                  pair-scores
                  pair-chosen-words)))
      (make-hash-table :test #'equal)))
