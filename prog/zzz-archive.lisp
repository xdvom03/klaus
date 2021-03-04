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


;; old convoluted pagerank, replaced by scoresum
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

;; Created while searching for the memory leak, should not be used (doesn't respect robots.txt)
(defun mockbot (seed queue-size page-count)
  (let ((queue (list seed))
        (acc nil)
        (visited-domains nil))
    (dotimes (i page-count)
      (print (concat "checking " (first queue)))
      (let* ((best-url (car queue))
             (raw-links (filter-links (vetted-links best-url)))
             (links (remove-duplicates (remove-if #'(lambda (url)
                                                      (or (member (core-domain (find-domain url))
                                                                  visited-domains
                                                                  :test #'equal)
                                                          (equal (core-domain (find-domain url))
                                                                 (core-domain (find-domain best-url)))))
                                                  raw-links)
                                       :test #'equal)))
        (push best-url acc)
        (push (core-domain (find-domain best-url)) visited-domains)
        (setf queue (cdr queue))
        (print (concat "found " (write-to-string (length raw-links)) " links, of which " (write-to-string (length links)) " will be checked"))
        (dolist (link links)
          (if (and (> queue-size (length queue))
                   (not (member link queue :test #'equal)))
              (push link queue)))))
    (reverse acc)))

(defun test (text)
  ;; what.
  (ltk:with-ltk ()
    (let ((w (window "blee"))
          (vocab (tokens text))
          (subfolders (subfolders "../DATA/classes/articles/")))
      (multiple-value-bind (scores pair-scores pair-words pair-word-scores) (scores vocab
                                                                                    subfolders
                                                                                    (map-to-hash #'get-recursive-corpus subfolders)
                                                                                    (map-to-hash #'get-word-count subfolders))
        (pair-scores-explainer 0 0 w vocab subfolders pair-scores pair-words pair-word-scores)))))

(defun remove-diacritics (str)
  (map 'string
       #'basic-letter
       str))

(defun basic-letter (ltr)
  (case ltr
    (#\á #\a)
    (#\é #\e)
    (#\ě #\e)
    (#\í #\i)
    (#\ó #\o)
    (#\ú #\u)
    (#\ů #\u)
    (#\ý #\y)
    (#\č #\c)
    (#\ď #\d)
    (#\ň #\n)
    (#\ř #\r)
    (#\š #\s)
    (#\ť #\t)
    (#\ž #\z)
    (#\RIGHT_SINGLE_QUOTATION_MARK #\')
    (otherwise ltr)))


;;; unsalvageably outdated crawler interface
(defun tick-compute (master queue queue-list acc acc-list visited-domains visited-list target queue-size)
  ;; BUG: Outdated compared to the fidgetbot (text interface).
  (ltk:destroy queue-list)
  (setf queue-list (scrollable-list 0 0 master *entries-per-page* queue))
  (ltk:destroy acc-list)
  (setf acc-list (scrollable-list 0 1 master *entries-per-page* acc))
  (ltk:destroy visited-list)
  (setf visited-list (scrollable-list 0 2 master *entries-per-page* visited-domains))
  (let* ((best-url (pick-from-queue queue))
         (raw-links (filter-links (vetted-links best-url)))
         (links (remove-if #'(lambda (link) (or (member (core-domain (find-domain link))
                                                        visited-domains
                                                        :test #'equal)
                                                (equal (core-domain (find-domain link))
                                                       (core-domain (find-domain best-url)))))
                           (mapcar #'render-wikipedia
                                   (remove-duplicates (remove-if-not #'(lambda (url) (and (url-allowed? *crawler-name* url)
                                                                                          (not (or (equal url best-url)
                                                                                                   (member url queue :test #'equal)
                                                                                                   (member url acc :test #'equal)
                                                                                                   (search "twitter." url)
                                                                                                   (search "facebook." url)
                                                                                                   (search "youtube." url)
                                                                                                   (search "google." url)
                                                                                                   (search "amazon." url)
                                                                                                   (search "instagram." url)))))
                                                                     raw-links)
                                                      :test #'equal)))))
    (push best-url acc)
    (push (core-domain (find-domain best-url)) visited-domains)
    #|(append-to-file (concat *crawl-data-folder* "focused-found")
    (concat (cdr (first (sort (copy-seq queue) #'> :key #'cdr))) " " best-url))|#
    (setf queue (remove-nth 0 (sort (copy-seq queue) #'> :key #'cdr)))
    (dolist (link links)
      (if (not (or (member link acc :test #'equal)
                   (member link queue :key #'car :test #'equal)))
          (let* ((worst-score (cdr (last1 queue)))
                 (score (link-score link target)))
            (if (equal (url-text link)
                       "nothingfound")
                (princ "x")
                (progn
                  (princ ".")
                                        ;(append-to-file (concat *crawl-data-folder* "focused-all") link)
                                        ;(append-to-file (concat *crawl-data-folder* "focused-all") score)
                  (if (> queue-size (length queue))
                      (setf queue (sort (copy-seq (append1 queue (cons link score))) #'> :key #'cdr))
                      (if (> score worst-score)
                          (setf queue (sort (copy-seq (replace-last queue (cons link score))) #'> :key #'cdr)))))))
          (princ "x")))
    (list master queue queue-list acc acc-list visited-domains visited-list target queue-size)))

;; only used for history
(defun file-lines (file)
  ;; UIOP for some reason always reads a blank first line
  ;; List of string lines
  (cdr (uiop:read-file-lines file)))

(defun file-name (path)
  (first (reverse (split path #\/))))

;; more outdated crawler
(defun tick-compute (master queue queue-list acc acc-list visited-domains visited-list target queue-size)
  ;; BUG: Outdated compared to the fidgetbot (text interface).
  (ltk:destroy queue-list)
  (setf queue-list (scrollable-list 0 0 master *entries-per-page* queue))
  (ltk:destroy acc-list)
  (setf acc-list (scrollable-list 0 1 master *entries-per-page* acc))
  (ltk:destroy visited-list)
  (setf visited-list (scrollable-list 0 2 master *entries-per-page* visited-domains))
  (let* ((best-url (pick-from-queue queue))
         (raw-links (filter-links (vetted-links best-url)))
         (links (remove-if #'(lambda (link) (or (member (core-domain (find-domain link))
                                                        visited-domains
                                                        :test #'equal)
                                                (equal (core-domain (find-domain link))
                                                       (core-domain (find-domain best-url)))))
                           (mapcar #'render-wikipedia
                                   (remove-duplicates (remove-if-not #'(lambda (url) (and (url-allowed? *crawler-name* url)
                                                                                          (not (or (equal url best-url)
                                                                                                   (member url queue :test #'equal)
                                                                                                   (member url acc :test #'equal)
                                                                                                   (search "twitter." url)
                                                                                                   (search "facebook." url)
                                                                                                   (search "youtube." url)
                                                                                                   (search "google." url)
                                                                                                   (search "amazon." url)
                                                                                                   (search "instagram." url)))))
                                                                     raw-links)
                                                      :test #'equal)))))
    (push best-url acc)
    (push (core-domain (find-domain best-url)) visited-domains)
    #|(append-to-file (concat *crawl-data-folder* "focused-found")
    (concat (cdr (first (sort (copy-seq queue) #'> :key #'cdr))) " " best-url))|#
    (setf queue (remove-nth 0 (sort (copy-seq queue) #'> :key #'cdr)))
    (dolist (link links)
      (if (not (or (member link acc :test #'equal)
                   (member link queue :key #'car :test #'equal)))
          (let* ((worst-score (cdr (last1 queue)))
                 (score (link-score link target)))
            (if (equal (url-text link)
                       "nothingfound")
                (princ "x")
                (progn
                  (princ ".")
                                        ;(append-to-file (concat *crawl-data-folder* "focused-all") link)
                                        ;(append-to-file (concat *crawl-data-folder* "focused-all") score)
                  (if (> queue-size (length queue))
                      (setf queue (sort (copy-seq (append1 queue (cons link score))) #'> :key #'cdr))
                      (if (> score worst-score)
                          (setf queue (sort (copy-seq (replace-last queue (cons link score))) #'> :key #'cdr)))))))
          (princ "x")))
    (list master queue queue-list acc acc-list visited-domains visited-list target queue-size)))

(defun tick (a master queue queue-list acc acc-list visited-domains visited-list target queue-size)
  (let ((data (tick-compute master queue queue-list acc acc-list visited-domains visited-list target queue-size)))
    (print a)
    (ltk:destroy queue-list)
    (setf queue-list (scrollable-list 0 0 master *entries-per-page* queue))
    (ltk:destroy acc-list)
    (setf acc-list (scrollable-list 0 1 master *entries-per-page* acc))
    (ltk:destroy visited-list)
    (setf visited-list (scrollable-list 0 2 master *entries-per-page* visited-domains))
    (if (not (zerop a))
        (ltk:after 25 #'(lambda () (apply #'tick (1- a) data))))))

(defun run-crawler (master seed page-count target queue-size)
  ;; TBD: Run this recursively as inspired by calc.
  (let ((queue-list (scrollable-list 0 0 master *entries-per-page* nil))
        (queue (list (cons seed 0)))
        (acc-list (scrollable-list 0 1 master *entries-per-page* nil))
        (acc nil)
        (visited-list (scrollable-list 0 2 master *entries-per-page* nil))
        (visited-domains nil))
    (tick page-count master queue queue-list acc acc-list visited-domains visited-list target queue-size)))

;; unused functions
(defun cut-word (word)
  (subseq word 0 (1- (length word))))

(defun last-char (str)
  (char str (1- (length str))))
