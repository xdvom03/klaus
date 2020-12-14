;; presumably removed - replaced by pair system
(defun evaluate (url folder &optional (on? t))
  ;; Has to return a list of conses (subfolder . score)
  ;; In folders without subfolders, we don't want to do anything
  (if (and on? (subfolders folder) *try-to-class?*)
      (let* ((subfolder-count (length (subfolders folder)))
             (subfolder-paths (subfolders folder))
             (smallest-subfolder (apply #'min (mapcar #'get-file-count subfolder-paths)))
             (vocab (gethash nil (wordlist (url-text url))))
             (corpuses (let ((acc (make-hash-table :test #'equal)))
                         (dolist (path subfolder-paths)
                           (setf (gethash path acc)
                                 (normalize-corpus (get-recursive-corpus path)
                                                   (/ smallest-subfolder (get-file-count path)) ; TBD: Warn if a folder is empty!
                                                   vocab)))
                         acc))
             (total-corpus (reduce #'add-hashtable-corpuses
                                   (mapcar #'(lambda (path) (gethash path corpuses))
                                           subfolder-paths)))
             (scores (make-hash-table :test #'equal))
             ;; path to score
             (final-scores (make-hash-table :test #'equal))
             (subfolder-chosen-words (make-hash-table :test #'equal)))
        
        (dolist (path subfolder-paths)
          ;; balance evidence for and against
          (let* ((corpus (gethash path corpuses))
                 ;; we need a score for every word-path pair
                 (word-scores (let ((acc (make-hash-table :test #'equal)))
                                (dolist (word vocab)
                                  (setf (gethash word acc) (word-probability (occurrences word corpus)
                                                                             (occurrences word total-corpus)
                                                                             subfolder-count)))
                                acc))
                 (ordered-words (sort (copy-seq vocab) #'< :key #'(lambda (word) (gethash word word-scores))))
                 (chosen-words (append (subseq ordered-words 0 (min *evidence-length*
                                                                    (length ordered-words)))
                                       (subseq ordered-words (max 0
                                                                  (- (length ordered-words) *evidence-length*)))))
                 (score (apply #'* (mapcar #'(lambda (word) (word-probability (occurrences word corpus)
                                                                              (occurrences word total-corpus)
                                                                              subfolder-count))
                                           ;; Need the best words here
                                           chosen-words))))
            (setf (gethash path scores) score)
            (setf (gethash path subfolder-chosen-words) chosen-words)))
        
        (let ((prob-sum (apply #'+ (mapcar #'(lambda (path) (gethash path scores)) subfolder-paths))))
          (dolist (path subfolder-paths)
            (setf (gethash path final-scores)
                  (/ (gethash path scores)
                     prob-sum))))
        
        ;; explainer-data needs a bunch of lists of (path . (STUFF)) with STUFF being displayed (so provide some useful data, as in all-scores)
        (if *explain?*
            (scores-explainer (mapcar #'(lambda (path) (cons path
                                                             (mapcar #'(lambda (word)
                                                                         (list (coerce (word-probability (occurrences word (gethash path corpuses))
                                                                                                         (occurrences word total-corpus)
                                                                                                         subfolder-count)
                                                                                       'single-float)
                                                                               word
                                                                               (coerce (occurrences word (gethash path corpuses)) 'single-float)
                                                                               (coerce (occurrences word total-corpus) 'single-float)))
                                                                     (gethash path subfolder-chosen-words))))
                                      subfolder-paths)))
        
        (mapcar #'(lambda (path)
                    (cons path
                          (coerce (gethash path final-scores) 'single-float)))
                subfolder-paths))
      ;; this is returned if evaluation is off
      (mapcar #'(lambda (path)
                  (cons path (/ (length (subfolders folder)))))
              (subfolders folder))))

;; This looks terrible and is hard to rewrite.
(defun pair-compare (vocab path1 path2)
  ;; simplified evaluate for two options
  ;; TBD: Clean up, maybe bring back the original evaluate
  (let* ((smallest-subfolder (min (get-file-count path1)
                                  (get-file-count path2)))
         (raw-corpus1 (get-recursive-corpus path1))
         (raw-corpus2 (get-recursive-corpus path2))
         (total-corpus (add-hashtable-corpuses raw-corpus1
                                               raw-corpus2))
         (corpus1 (normalize-corpus raw-corpus1
                                    (/ smallest-subfolder (get-file-count path1)) ; TBD: Warn if a folder is empty!
                                    vocab))
         (corpus2 (normalize-corpus raw-corpus2
                                    (/ smallest-subfolder (get-file-count path2)) ; TBD: Warn if a folder is empty!
                                    vocab))
         (word-scores1 (let ((acc (make-hash-table :test #'equal)))
                         (dolist (word vocab)
                           (setf (gethash word acc) (word-probability (occurrences word corpus1)
                                                                      (occurrences word total-corpus)
                                                                      2)))
                         acc))
         (word-scores2 (let ((acc (make-hash-table :test #'equal)))
                         (dolist (word vocab)
                           (setf (gethash word acc) (word-probability (occurrences word corpus2)
                                                                      (occurrences word total-corpus)
                                                                      2)))
                         acc))
         (ordered-words1 (sort (copy-seq vocab) #'< :key #'(lambda (word) (gethash word word-scores1))))
         (ordered-words2 (sort (copy-seq vocab) #'< :key #'(lambda (word) (gethash word word-scores2))))
         (chosen-words1 (append (subseq ordered-words1 0 (min *evidence-length*
                                                              (length ordered-words1)))
                                (subseq ordered-words1 (max 0
                                                            (- (length ordered-words1) *evidence-length*)))))
         (chosen-words2 (append (subseq ordered-words2 0 (min *evidence-length*
                                                              (length ordered-words2)))
                                (subseq ordered-words2 (max 0
                                                            (- (length ordered-words2) *evidence-length*)))))
         (score1 (apply #'* (mapcar #'(lambda (word) (word-probability (occurrences word corpus1)
                                                                       (occurrences word total-corpus)
                                                                       2))
                                    ;; Need the best words here
                                    chosen-words1)))
         (score2 (apply #'* (mapcar #'(lambda (word) (word-probability (occurrences word corpus2)
                                                                       (occurrences word total-corpus)
                                                                       2))
                                    ;; Need the best words here
                                    chosen-words2)))
         (prob-sum (+ score1 score2))
         (final-score1 (/ score1 prob-sum)))
    ;; TBD: Rework explainer for pairs
    ;; Only returns the score for the first folder
    (coerce final-score1 'single-float)))

(defun folder-index (path)
  (let ((split-path (split path (char "/" 0))))
    (read-from-string (nth (- (length split-path) 2)
                           split-path))))

(defun decrement-folder (path)
  (let ((split-link (split path (char "/" 0))))
    (setf (nth (- (length split-link) 2) split-link)
          (write-to-string (1- (folder-index path))))
    (reduce #'(lambda (a b) (concat a "/" b)) split-link)))

(defun count-between (a b)
  (let ((acc nil))
    (dotimes (i (- (1+ b) a))
      (push (+ a i) acc))
    (reverse acc)))

(defun file-url (file) ;; Beware, this is required for link window! Change that!
  (read-from-file (concat (namestring file) "url")))

(defun file-texts (folder)
  (mapcar #'(lambda (file-name) (extract-text (file-content file-name)))
          (class-links folder)))

(defun links (file-name)
  ;; TBD: Why is the file read differently here?
  (with-open-file (stream file-name :direction :input)
    (let ((str (make-string (file-length stream))))
      (read-sequence str stream)
      (mapcar #'(lambda (str)
                  (subseq str 1 (- (length str) 2)))
              (remove-if #'(lambda (str) (equal str ""))
                         (split str #\Newline))))))

(defun get-subfolder-corpus (folder)
  ;; Like get-recursive-corpus, but without the files in the folder itself.
  (corpus-subtract (get-recursive-corpus folder)
                   (get-corpus folder)))

(defun corpus-subtract (corp1 corp2)
  ;; Works with hash tables
  ;; Assumes that corp2 is a subcorpus of corp2, so there is nothing negative in the result (and the vocabulary of corp2 is included in the vocabulary of corp1)
  ;; No need to remove words with 0 occurrences. TBD: In case of bottleneck, check if it makes the program faster.
  (let ((acc (map-to-hash #'(lambda (word) (- (occurrences word corp1)
                                              (occurrences word corp2)))
                          (list-keys corp1))))
    acc))
