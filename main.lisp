(defun list-hashes (hashtable)
  (let ((acc nil))
    (maphash #'(lambda (a b) (push (cons a b) acc))
             hashtable)
    acc))
#|
State of the union: New backend kinda sorta works, but a lot of things are fragile, scoring is untested and the explainer has display problems. Conduct a code review, refactor.

PROBLEM: A few tiny folders can mess up the confidence for the whole superfolder. Add more data.
TBD: Allow for moving between pages of words in evidence, provide more data. Boilerplate always creates an unreadable mountain of evidence.
BUG: Turning off classing causes crashes.
Convergence gets slow for very obvious choices, like boilerplace. We lose 30 orders of magnitude of certainty. Probably fine, for we are still sure.
TBD: Display bottlenecks for precision (subfolder with fewest files).
TBD: Toggle adding into history
TBD: Speed up
TBD: macro-fy the list window or rework the whole damn thing
TBD: Load title for links
BUG: The method with the 0.8 threshold causes some sites to be classed based on just one word. Shucks, should take flimsier evidence?
BUG: Fails to recognize URLs as identical given minor alterations (such as www, http/s, etc.). Perhaps something like a "purge duplicates" button would be useful, looking at page contents, and thus recognising reposts.
TBD: Generalise the list window so that it is not made separately for history, explainers, 
TBD: Autoclose explainers when a thing is classed.
TBD: Consider a category an average of its subcategories, not files. This would help protect against availability bias (say, Lifehack taking over trash) and make the system more semantic. 
BUG: Some apostrophes are ', some are &rsquo;, they class very differently and should not matter almost at all.
TBD: Class OHYOS.

Proof against tends to be a stronger discriminator than proof for. Is that a problem? It helps distinguish, say, articles about copyright from legal boilerplate. But what about the general case?

Possibly put into documentation:
The starting threeway decision tends to go like this:
We know it's not boilerplate because of a bunch of meaningful words.
There are a couple trash-y words, but that might get fixed as I add more data. Mostly errors.
It seems not to be valuable because I can find a couple boilerplate-y words. These tend to (maybe?) class more in trash than valuable.
We get tied trash and valuable, but the boilerplate-y words should not be considered at all because they can only be admitted as evidence for the boilerplate theory.
Check out if a system that weighs word occurrences by the scores of their corresponding categories converges to a solution.

What do we do with <noscript>? It caused c2wiki to class as boilerplate, but I think there's no good reason to exclude it. Made irrelevant by C2wiki depending on Javascript, and thus not being downloadable.
Related TBD: Redownload button.
|#

(defun words-explainer (words word-scores page-length)
  (let* ((W (window "explaining word scores or something since 17-10-2020 or sometime"))
         (f (frame 0 0 W))
         (bottom-frame (frame 1 0 W))
         (l (label 2 0 W ""))
         ;; BUG: We cannot rely on words for being actually for (kde nic neni, ani smrt nebere, treba Lifehack hodnotny)
         (words-for (reverse (remove-if #'(lambda (word) (< (gethash word word-scores) 0.5))
                                        words)))
         (words-against (remove-if #'(lambda (word) (> (gethash word word-scores) 0.5))
                                   words))
         (buttons (button-column f 0 page-length 1))
         (buttons2 (button-column f 1 page-length 1))
         (maximum (max (length words-for)
                       (length words-against)))
         (start 0)
         left
         right)
    (labels ((redraw ()
               (dotimes (i page-length)
                 (let ((word-num (+ start i)) ;rebind
                       (b (nth i buttons))
                       (b2 (nth i buttons2)))
                   (setf (ltk:text b) (if (> (length words-for) word-num)
                                          (concat (nth word-num words-for) " " (write-to-string (coerce (gethash (nth word-num words-for) word-scores) 'single-float)))
                                          ""))
                   (setf (ltk:text b2) (if (> (length words-against) word-num)
                                           (concat (nth word-num words-against) " " (write-to-string (coerce (gethash (nth word-num words-against) word-scores) 'single-float)))
                                           ""))))
               (setf (ltk:text l) (concat (write-to-string start) "/" (write-to-string maximum)))
               (if (>= start page-length)
                   (ltk:configure left :state :normal)
                   (ltk:configure left :state :disabled))
               (if (<= start (- maximum page-length))
                   (ltk:configure right :state :normal)
                   (ltk:configure right :state :disabled))))
      (setf left (button 0 0 bottom-frame "←" #'(lambda ()
                                                  (decf start page-length)
                                                  (redraw))))
      (setf right (button 0 1 bottom-frame "→" #'(lambda ()
                                                   (incf start page-length)
                                                   (redraw))))
      (redraw)
      (button 0 0 f "EVIDENCE FOR" #'pass)
      (button 0 1 f "EVIDENCE AGAINST" #'pass)
      W)))

(defun pair-scores-explainer (vocab folders pair-scores pair-chosen-words) ; TBD: Fix names
  ;; The scores FOR a given folder are in its rows. TBD: Make that clear from the window
  (let* ((W (window "explaining scores since 17-10-2020")))
    (dotimes (i (length folders))
      (let ((folder (nth i folders)))
        (button 0 (1+ i) W (simplified-path folder) #'pass)
        (button (1+ i) 0 W (simplified-path folder) #'pass)
        (dotimes (j (length folders))
          (let* ((opponent (nth j folders))
                 (smaller-size (min (get-file-count folder)
                                    (get-file-count opponent)))
                 (folder-corpus (normalize-corpus (get-recursive-corpus folder)
                                                  (/ smaller-size (get-file-count folder))
                                                  vocab))
                 (opponent-corpus (normalize-corpus (get-recursive-corpus opponent)
                                                    (/ smaller-size (get-file-count opponent))
                                                    vocab)) ;; TBD: Rework code to make that [whether a corpus is normalized or not] more obvious next time. TBD: Don't request the full vocab.
                 (pair (cons folder opponent))
                 (chosen-words (gethash pair pair-chosen-words))
                 (word-scores (let ((acc (make-hash-table :test #'equal)))
                                (dolist (word chosen-words)
                                  (setf (gethash word acc)
                                        (word-probability (occurrences word folder-corpus)
                                                          (+ (occurrences word folder-corpus)
                                                             (occurrences word opponent-corpus))
                                                          2)))
                                acc))
                 (score (gethash pair pair-scores)))
            (button (1+ i)
                    (1+ j)
                    W
                    (if (= i j)
                        ""
                        (write-to-string score))
                    #'(lambda () (words-explainer chosen-words word-scores *entries-per-page*)))))))
    W))

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
                                       (- (length ordered-words) evidence-length))))

    #|(remove-if #'(lambda (word) (> 0.8 (gethash word word-scores) 0.2))
    ordered-words)|#
    ))

(defun compare-folders (vocab folders)
  ;; Returns a cons of two hash tables. A hash table of path -> score, and a hash table of path -> chosen words.
  (let* ((folder-count (length folders))
         (smallest-folder (apply #'min (mapcar #'get-file-count folders)))
         (corpuses (let ((acc (make-hash-table :test #'equal)))
                     (dolist (path folders)
                       (setf (gethash path acc)
                             (normalize-corpus (get-recursive-corpus path)
                                               (/ smallest-folder (get-file-count path)) ; TBD: Warn if a folder is empty!
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
                                                      folder-count)))
                            acc))
             (chosen-words (chosen-words vocab word-scores))
             (score (apply #'* (mapcar #'(lambda (word) (gethash word word-scores))
                                       ;; Need the best words here
                                       chosen-words))))
        (apply #'* (mapcar #'(lambda (word) (coerce (gethash word word-scores) 'double-float))
                           ;; Need the best words here
                           chosen-words))
        (setf (gethash path scores) score)
        (setf (gethash path evidence) chosen-words)))

    ;; Potential BUG: Very long ratios
    (let ((prob-sum (apply #'+ (mapcar #'(lambda (path) (gethash path scores)) folders))))
      (dolist (path folders)
        (setf (gethash path scores)
              (coerce (/ (gethash path scores)
                         prob-sum)
                      'single-float))))
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
                (let* ((data (compare-folders vocab (list folder opponent)))
                       (scores (car data))
                       (evidence (cdr data)))
                  (setf (gethash (cons folder opponent) pair-scores)
                        (gethash folder scores))
                  (setf (gethash (cons folder opponent) pair-chosen-words)
                        (gethash folder evidence))))))
        (let ((scores (pagerank folders pair-scores)))
          ;; the explainer should get two major word info tables, coded as hash tables for (folder . opponent), and containing either hash tables of word -> score or lists of decisive words.
          (list-hashes pair-chosen-words)
          (if *explain?*
              (pair-scores-explainer vocab folders pair-scores pair-chosen-words))
          scores))
      (make-hash-table :test #'equal)))

(defun database-window (url-entry)
  (let* ((url (ltk:text url-entry))
         (W (make-instance 'ltk:toplevel :title "DATABASE"))
         (current-folder *classes-folder*)
         (counter 0)
         (widget-list nil)
         (f (frame 0 0 W)) ; frame for everything except the comment
         (comment-frame (frame 0 1 W))
         (e (entry 2 2 f))
         (tex (text 0 0 comment-frame "")))
    (labels ((redraw-confirmed (new-path)
               (setf current-folder new-path)
               (setf (ltk:text tex) (read-comment current-folder))
               (dolist (i widget-list)
                 (ltk:destroy i))
               (setf widget-list nil)
               (setf counter 1)
               (push (label 0 2 f (concat "Current folder: " current-folder))
                     widget-list)
               ;; produces conses of (subfolder . score)
               (let* ((subfolders (subfolders current-folder))
                      (scores (scores (gethash nil (wordlist (url-text url))) subfolders)))
                 (dolist (i subfolders)
                   (incf counter)
                   (push (button counter
                                 1
                                 f
                                 (concat (file-name i t)
                                         " score: "
                                         (if (gethash i scores)
                                             (gethash i scores)
                                             "unknown")
                                         ", "
                                         (get-file-count i)
                                         " files.")
                                 #'(lambda ()
                                     (redraw i)))
                         widget-list))))

             (redraw (new-path)
               ;; Reading from the text adds a newline. Unclear why.
               (if (equal (concat (read-comment current-folder)
                                  *newline*)
                          (ltk:text tex))
                   (redraw-confirmed new-path)
                   (let ((warning-button nil))
                     (setf warning-button (button 2 0 comment-frame "Change folder despite unsaved comment" #'(lambda ()
                                                                                                                (redraw-confirmed new-path)
                                                                                                                (ltk:destroy warning-button))))))))
      (button 0 0 f "X" #'kill-all)
      (label 0 1 f "FOLDERS")
      (button 3 2 f "Create folder" #'(lambda () (let ((txt (ltk:text e)))
                                                   (ensure-directories-exist (concat current-folder txt "/"))
                                                   (create-file (concat current-folder txt "/corpus") nil)
                                                   (create-file (concat current-folder txt "/file-count") 0)
                                                   (create-file (concat current-folder txt "/links") nil)
                                                   (setf (ltk:text e) "")
                                                   (redraw (concat current-folder txt "/")))))
      (button 1 1 f ".." #'(lambda () (redraw (parent-folder current-folder))))
      (label 1 2 f (concat "Current URL: " url))
      (button 4 2 f "Add link here" #'(lambda ()
                                        ;; Avoiding double insert
                                        (if (not (member url (file-urls current-folder) :test #'equal))
                                            (progn
                                              (log-print (concat "Adding URL " url " to folder " (simplified-path current-folder)))
                                              (let* ((links-file (concat current-folder "links"))
                                                     (existing-links (with-open-file (stream links-file)
                                                                       (read stream))))
                                                (redownload-file url)
                                                (with-open-file (stream links-file :direction :output :if-exists :supersede :if-does-not-exist :create)
                                                  (print (append1 existing-links url) stream)))
                                              (setf (ltk:text url-entry) "")
                                              (ltk:destroy W))
                                            (log-print "File already in folder."))))
      (button 5 2 f "List links in this folder" #'(lambda ()
                                                    (list-window (file-urls current-folder)
                                                                 (mapcar #'(lambda (link) #'(lambda () (link-options-window (car link) (cdr link) (concat current-folder "files/") url-entry)))
                                                                         (mapcar #'cons
                                                                                 (file-urls current-folder)
                                                                                 (mapcar #'1+ (countup (length (file-urls current-folder))))))
                                                                 *entries-per-page*
                                                                 "Links")))
      (button 1 0 comment-frame "Save comment" #'(lambda ()
                                                   (log-print "Saved comment in " (simplified-path current-folder))
                                                   (set-comment (ltk:text tex) current-folder)))
      (redraw-confirmed current-folder))))

(defun folder-index (path)
  (let ((split-path (split path (char "/" 0))))
    (read-from-string (nth (- (length split-path) 2)
                           split-path))))

(defun decrement-folder (path)
  (let ((split-link (split path (char "/" 0))))
    (setf (nth (- (length split-link) 2) split-link)
          (write-to-string (1- (folder-index path))))
    (reduce #'(lambda (a b) (concat a "/" b)) split-link)))

(defun link-options-window (link index folder url-entry)
  (let* ((W (window link))
         (link-folder (concat folder (write-to-string index) "/"))
         (url (file-url link-folder)))
    (button 0 0 W "REMOVE" #'(lambda ()
                               ;; For safety reasons, no recursive deletion is used.
                               (delete-file (concat link-folder "url"))
                               (delete-file (concat link-folder "html"))
                               (delete-file (concat link-folder "text"))
                               (delete-directory link-folder)
                               ;; Then decrement all further folders
                               (dolist (other-link (directory* folder))
                                 (if (> (folder-index (namestring other-link))
                                        index)
                                     (rename-file other-link (decrement-folder (namestring other-link)))))
                               (ltk:destroy W)
                               (log-print "Deleted file in folder " (write-to-string folder) " with url: " url)))
    (button 1 0 W "LOAD" #'(lambda () (setf (ltk:text url-entry) link) (ltk:destroy W)))))

(defun link-window (entry)
  (let ((links (vetted-links (ltk:text entry)
                             (find-domain (ltk:text entry)))))
    (list-window links
                 (mapcar #'(lambda (link) (setf (ltk:text entry) link))
                         links)
                 *entries-per-page*
                 "Linkz")))

(defun run ()
  (ltk:with-ltk ()
    (ltk:withdraw ltk:*tk*)
    (let* ((W (window "klaus"))
           (LW (window "LOG"))
           (e1 (entry 0 1 W))
           (log-list nil)
           ch
           ch2)
      (defun log-print (&rest strings)
        ;; BUG: Log entry seems to show with a lag.
        (let ((full-string (apply #'concat strings)))
          (push (button (length log-list) 0 LW full-string #'pass) log-list)
          full-string))
      (defun kill-all ()
        (ltk:destroy W)
        (ltk:destroy ltk:*tk*))
      (ltk:focus LW)
      (ltk:on-close W #'(lambda () (ltk:destroy ltk:*tk*)))
      (button 0 1 LW "Wipe log" #'(lambda ()
                                    (dolist (button log-list)
                                      (ltk:destroy button))
                                    (setf log-list nil)))
      (button 0 2 W "Open & edit history" #'(lambda () (history-window e1 *entries-per-page*)))
      (button 1 1 W "List links" #'(lambda ()
                                     (link-window e1)))
      (button 2 1 W "Random link" #'(lambda ()
                                      (let* ((url (ltk:text e1))
                                             (links (vetted-links url (find-domain url)))
                                             (new-link (nth (random (length links)) links)))
                                        (setf (ltk:text e1) new-link))))
      (button 3 1 W "Open in Firefox" #'(lambda ()
                                          (open-url (ltk:text e1))))
      (button 4 1 W "Open database" #'(lambda ()
                                        (add-to-history (ltk:text e1))
                                        (database-window e1)))
      (button 1 2 W "Wipe entry" #'(lambda ()
                                     (setf (ltk:text e1) "")))
      (button 0 0 W "X" #'kill-all)
      (button 1 3 W "Rebuild corpus" #'rebuild-corpus)

      (setf ch (checkbox 2 3 w "Try to class?" #'(lambda (a)
                                                   a
                                                   (setf *try-to-class?* (ltk:value ch))
                                                   (setf (ltk:value ch) *try-to-class?*))))
      (setf (ltk:value ch) *try-to-class?*)
      (setf ch2 (checkbox 3 3 w "Explain classing?" #'(lambda (a)
                                                        a
                                                        (setf *explain?* (ltk:value ch2))
                                                        (setf (ltk:value ch2) *explain?*))))
      (setf (ltk:value ch2) *explain?*)
      (log-print "Launched main menu."))))
