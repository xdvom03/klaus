#|
State of the union: New backend kinda sorta works, but a lot of things are fragile, scoring is untested and the explainer has display problems. Conduct a code review, refactor.

PROBLEM: A few tiny folders can mess up the confidence for the whole superfolder. Add more data.
Convergence gets slow for very obvious choices, like boilerplace. We lose 30 orders of magnitude of certainty. Probably fine, for we are still sure. TBD: Make the algorithm wait for having converged.
TBD: Toggle adding into history
TBD: Speed up
TBD: Load title for links
TBD: Autoclose explainers when a thing is classed.
TBD: Consider a category an average of its subcategories, not files. This would help protect against availability bias (say, Lifehack taking over trash) and make the system more semantic. 
TBD: Display where word scores came from (target/total normalised occurrences)

Proof against tends to be a stronger discriminator than proof for. Is that a problem? It helps distinguish, say, articles about copyright from legal boilerplate. But what about the general case?
This seems fixed by symmetry in the simpler two-class case: proof for IS proof against of the other folder.

What do we do with <noscript>? It caused c2wiki to class as boilerplate, but I think there's no good reason to exclude it. Made irrelevant by C2wiki depending on Javascript, and thus not being downloadable.

Possible windows:
Main menu
Database (classes)
History
Links

|#

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
        ;; the actual scores and some data for the explainer
        (values (pagerank folders pair-scores) 
                pair-scores
                pair-chosen-words))
      (make-hash-table :test #'equal)))

;;; NORMAL STUFFS
;;;----------------------------------------------------------------------------------------------
;;; GUI

(defun words-explainer (r c master words word-scores page-length)
  ;; TBD: This is suboptimal, the two lists are considered separate when they are really supposed to balance each other.
  (let* ((f (frame r c master))
         ;; BUG: We cannot rely on words for being actually for (kde nic neni, ani smrt nebere, treba Lifehack hodnotny)
         (words-for (reverse (remove-if #'(lambda (word) (< (gethash word word-scores) 0.5))
                                        words)))
         (words-against (remove-if #'(lambda (word) (> (gethash word word-scores) 0.5))
                                   words)))
    (scrollable-list 0 0 f page-length words-for)
    (scrollable-list 0 1 f page-length words-against)))

(defun pair-scores-explainer (r c master vocab folders pair-scores pair-chosen-words) ; TBD: Fix names
  ;; The scores FOR a given folder are in its rows. TBD: Make that clear from the window
  (let* ((f (frame r c master)))
    (dotimes (i (length folders))
      (let ((folder (nth i folders)))
        (button 0 (1+ i) f (simplified-path folder) #'pass)
        (button (1+ i) 0 f (simplified-path folder) #'pass)
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
                    f
                    (if (= i j)
                        ""
                        (write-to-string score))
                    #'(lambda () (words-explainer 10 10 f chosen-words word-scores *entries-per-page*)))))))
    f))

(defun run ()
  #|
  TBD: Hide all the state and windows here, then rename
  TBD (marked): The total window is redundantly organised. The new screen should always grid it itself, at (0 1), but all the subwindows already grid everything themselves.
                Thus, the (0 1) is awkwardly present through all the subwindows.
  TBD: Make it impossible to leave the parent class
  TBD: This is a mess. Read up on C2 Wiki GUI design, see if this can be done more functionally.
  BUG: The explainers are not properly placed. Related to marked TBD.
  |#
  (ltk:with-ltk ()
    (ltk:withdraw ltk:*tk*)
    (let* ((W (window "klaus"))
           (displayed-frame nil)
           (current-url ""))
      (ltk:on-close W #'(lambda () (ltk:destroy ltk:*tk*)))
      (labels ((change-screen (new-frame)
                 (if displayed-frame
                     (ltk:destroy displayed-frame))
                 (setf displayed-frame new-frame))

               (link-window (r c master url)
                 (let ((links (vetted-links url)))
                   (scrollable-list r c master *entries-per-page* links
                                    (mapcar #'(lambda (link) (setf current-url link))
                                            links))))
               
               (main-menu (r c master)
                 (let* ((W (frame r c master))
                        (e1 (entry 0 1 W))
                        ch
                        ch2)
                   (setf (ltk:text e1) current-url)
                   (button 0 2 W "Open & edit history" #'(lambda () (change-screen (history-window 0 1 master *entries-per-page*))))
                   (button 1 1 W "List links" #'(lambda ()
                                                  (change-screen (link-window 0 1 master current-url))))
                   (button 2 1 W "Random link" #'(lambda ()
                                                   (let* ((url (ltk:text e1))
                                                          (links (vetted-links url))
                                                          (new-link (nth (random (length links)) links)))
                                                     (setf (ltk:text e1) new-link))))
                   (button 3 1 W "Open in Firefox" #'(lambda ()
                                                       (open-url (ltk:text e1))))
                   (button 4 1 W "Open database" #'(lambda ()
                                                     (setf current-url (ltk:text e1))
                                                     (add-to-history current-url)
                                                     (change-screen (database-window 0 1 master current-url))))
                   (button 1 2 W "Wipe entry" #'(lambda ()
                                                  (setf (ltk:text e1) "")))
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
                   (log-print "Launched main menu.")
                   W))
               
               (database-window (r c master url)
                 (let* ((fr (frame r c master))
                        (current-folder *classes-folder*)
                        (counter 0)
                        (widget-list nil)
                        (f (frame 0 0 fr)) ; frame for everything except the comment
                        (comment-frame (frame 0 1 fr))
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
                                     (vocab (gethash nil (wordlist (url-text url)))))
                                (multiple-value-bind (scores pair-scores pair-chosen-words) (scores vocab subfolders)
                                  (if *explain?*
                                      (pair-scores-explainer 0 0 fr vocab subfolders pair-scores pair-chosen-words))
                                  (dolist (i subfolders)
                                    (incf counter)
                                    (push (button counter
                                                  1
                                                  f
                                                  (concat (file-name i t) " score: " (fallback (gethash i scores) "unknown") ", " (get-file-count i) " files.")
                                                  #'(lambda ()
                                                      (redraw i)))
                                          widget-list)))))

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
                     (label 0 1 f "FOLDERS")
                     (button 3 2 f "Create folder" #'(lambda () (let ((txt (ltk:text e)))
                                                                  (ensure-directories-exist (concat current-folder txt "/"))
                                                                  (overwrite-file (concat current-folder txt "/corpus") nil)
                                                                  (overwrite-file (concat current-folder txt "/file-count") 0)
                                                                  (overwrite-file (concat current-folder txt "/links") nil)
                                                                  (setf (ltk:text e) "")
                                                                  (redraw (concat current-folder txt "/")))))
                     (button 1 1 f ".." #'(lambda () (redraw (parent-folder current-folder))))
                     (label 1 2 f (concat "Current URL: " url))
                     (button 4 2 f "Add link here" #'(lambda ()
                                                       ;; Avoiding double insert
                                                       (if (not (member url (class-links current-folder) :test #'equal))
                                                           (let* ((links-file (concat current-folder "links"))
                                                                  (existing-links (read-from-file links-file)))
                                                             (log-print (concat "Adding URL " url " to folder " (simplified-path current-folder)))
                                                             (redownload-file url)
                                                             (overwrite-file links-file (append1 existing-links url))
                                                             (setf current-url "")
                                                             (ltk:destroy fr))
                                                           (log-print "File already in folder."))))
                     (button 5 2 f "List links in this folder" #'(lambda ()
                                                                   (let ((links (class-links current-folder)))
                                                                     (change-screen (scrollable-list 0 1 master *entries-per-page* links
                                                                                                     (mapcar #'(lambda (link)
                                                                                                                 #'(lambda () (link-options-window link (concat current-folder "links"))))
                                                                                                             links))))))
                     (button 1 0 comment-frame "Save comment" #'(lambda ()
                                                                  (log-print "Saved comment in " (simplified-path current-folder))
                                                                  (set-comment (ltk:text tex) current-folder)))
                     (redraw-confirmed current-folder)
                     fr)))
               
               (link-options-window (url file)
                 (let* ((W (window url)))
                   (button 0 0 W "REMOVE" #'(lambda ()
                                              (overwrite-file file (remove-if #'(lambda (checked-url)
                                                                                  (equal checked-url url))
                                                                              (read-from-file file)))
                                              (ltk:destroy W)
                                              (back-to-main)
                                              (log-print "Deleted file in folder " (write-to-string file) " with url: " url)))
                   (button 1 0 W "LOAD" #'(lambda ()
                                            (setf current-url url)
                                            (back-to-main)
                                            (ltk:destroy W)))))

               (history-window (r c master page-length)
                 ;; TBD: Use the list scrollable stuff.
                 (let* ((fr (frame r c master))
                        (e (entry 0 3 fr))
                        (f (frame page-length 1 fr))
                        (l (label (1+ page-length) 1 fr ""))
                        (buttons (button-column fr 1 page-length))
                        (buttons2 (button-column fr 2 page-length))
                        (start 0)
                        left
                        right)
                   (labels ((redraw ()
                              (let ((hst (history)))
                                (dotimes (i page-length)
                                  (let ((open-link (+ start i)) ;rebind
                                        (b (nth i buttons)))
                                    (if (> (length hst) (+ start i) -1)
                                        (progn
                                          (setf (ltk:text b) (nth (+ start i) hst))
                                          (setf (ltk:command b) #'(lambda () (setf current-url (nth open-link hst)))))
                                        (progn
                                          (setf (ltk:text b) "")
                                          (setf (ltk:command b) #'pass)))))
                                (dotimes (i page-length)
                                  (let ((remove-link (+ start i)) ; rebind
                                        (b (nth i buttons2)))
                                    (if (> (length hst) (+ start i) -1)
                                        (progn
                                          (setf (ltk:text b) "REMOVE")
                                          (setf (ltk:command b) #'(lambda ()
                                                                    (remove-from-history remove-link)
                                                                    (redraw))))
                                        (progn
                                          (setf (ltk:text b) "")
                                          (setf (ltk:command b) #'pass)))))
                                (setf (ltk:text l) (concat start "/" (length hst)))
                                (if (>= start page-length)
                                    (ltk:configure left :state :normal)
                                    (ltk:configure left :state :disabled))
                                (if (<= start (- (length hst) page-length))
                                    (ltk:configure right :state :normal)
                                    (ltk:configure right :state :disabled)))))
                     (setf left (button 0 0 f "←" #'(lambda ()
                                                      (decf start page-length)
                                                      (redraw))))
                     (setf right (button 0 1 f "→" #'(lambda ()
                                                       (incf start page-length)
                                                       (redraw))))
                     (redraw)
                     (button 1 3 fr "Push to history" #'(lambda ()
                                                          (add-to-history (ltk:text e)) (redraw)))
                     fr)))
               
               (back-to-main ()
                 (change-screen (main-menu 0 1 W))))
        (let ((log (frame 0 1000 W))
              (log-list nil))
          (button 0 1 log "Wipe log" #'(lambda ()
                                         (dolist (button log-list)
                                           (ltk:destroy button))
                                         (setf log-list nil)))
          (defun log-print (&rest strings)
            ;; BUG: Log entry seems to show with a lag.
            (let ((full-string (apply #'concat strings)))
              (push (button (length log-list) 0 log full-string #'pass) log-list)
              full-string)))
        (button 0 0 W "X" #'back-to-main)
        (back-to-main)))))
