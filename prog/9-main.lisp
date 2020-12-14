#|
PROBLEM: A few tiny folders can mess up the confidence for the whole superfolder. Add more data.
Convergence gets slow for very obvious choices, like boilerplace. We lose 30 orders of magnitude of certainty. Probably fine, for we are still sure. TBD: Make the algorithm wait for having converged.
TBD: Toggle adding into history
TBD: Speed up
TBD: Load title for links
TBD: Autoclose explainers when a thing is classed.
TBD: Consider a category an average of its subcategories, not files. This would help protect against availability bias (say, Lifehack taking over trash) and make the system more semantic.

What do we do with <noscript>? It caused c2wiki to class as boilerplate, but I think there's no good reason to exclude it. Made irrelevant by C2wiki depending on Javascript, and thus not being downloadable.

Some sites use scripts to deliver boilerplate. While this is not a problem for classification (just ignore them or input manually), it might mess up the crawler.
|#

;;; NORMAL STUFFS
;;;----------------------------------------------------------------------------------------------
;;; GUI

(defun words-explainer (r c master words word-scores folder-corpus opponent-corpus page-length)
  ;; TBD: Move the exps to a more reasonable place
  (let* ((f (frame r c master))
         ;; BUG: We cannot rely on words for being actually for (kde nic neni, ani smrt nebere, treba Lifehack hodnotny)
         (words-for (reverse (remove-if #'(lambda (word) (< (exp (gethash word word-scores)) 0.5))
                                        words)))
         (words-against (remove-if #'(lambda (word) (> (exp (gethash word word-scores)) 0.5))
                                   words)))
    (button 0 0 f "words for" #'pass)
    (button 0 1 f "words against" #'pass)
    (scrollable-list 1 0 f page-length (mapcar #'list words-for
                                               (mapcar #'(lambda (word) (coerce (exp (gethash word word-scores)) 'single-float)) words-for)
                                               (mapcar #'(lambda (word) (coerce (occurrences word folder-corpus) 'single-float)) words-for)
                                               (mapcar #'(lambda (word) (coerce (occurrences word opponent-corpus) 'single-float)) words-for)))
    (scrollable-list 1 1 f page-length (mapcar #'list words-against
                                               (mapcar #'(lambda (word) (coerce (exp (gethash word word-scores)) 'single-float)) words-against)
                                               (mapcar #'(lambda (word) (coerce (occurrences word folder-corpus) 'single-float)) words-against)
                                               (mapcar #'(lambda (word) (coerce (occurrences word opponent-corpus) 'single-float)) words-against)))))

(defun folder-name (path)
  (second (reverse (split path #\/))))

(defun pair-scores-explainer (r c master vocab folders pair-scores pair-words pair-word-scores) ; TBD: Fix names
  ;; The scores FOR a given folder are in its rows. TBD: Make that clear from the window
  (let* ((f (frame r c master)))
    (dotimes (i (length folders))
      (let ((folder (nth i folders)))
        (button 0 (1+ i) f (folder-name folder) #'pass)
        (button (1+ i) 0 f (folder-name folder) #'pass)
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
                 (chosen-words (gethash pair pair-words))
                 (word-scores (gethash pair pair-word-scores))
                 (score (gethash pair pair-scores)))
            (button (1+ i)
                    (1+ j)
                    f
                    (if (= i j)
                        ""
                        (write-to-string (my-round score)))
                    #'(lambda () (words-explainer 100 100 (window "hujaja") chosen-words word-scores folder-corpus opponent-corpus *entries-per-page*)))))))
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
                                                  (change-screen (link-window 0 1 master (ltk:text e1)))))
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
                   (button 5 1 W "Find word counts" #'(lambda ()
                                                        (if (gethash (ltk:text e1) (get-recursive-corpus *classes-folder*))
                                                            (change-screen (word-explainer 0 1 master (ltk:text e1)))
                                                            (log-print "Not a word."))))
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
                   W))
               
               (database-window (r c master url)
                 (let* ((fr (frame r c master))
                        (current-folder *classes-folder*)
                        (widget-list nil)
                        
                        (folder-frame (frame 0 0 fr))
                        (file-frame (frame 0 1 fr))
                        (options-frame (frame 1 0 fr))
                        (comment-frame (frame 1 1 fr))

                        (vocab (if *try-to-class?* (tokens (url-text url))))

                        ;; variable stuff
                        (e (entry 2 0 options-frame))
                        (tex (text 0 0 comment-frame ""))
                        (probsum-label (label 5 0 options-frame ""))
                        (folder-label (label 0 0 options-frame ""))
                        (file-list (frame 0 1 file-frame)))
                   (labels ((redraw-confirmed (new-path)
                              (setf current-folder new-path)
                              (setf (ltk:text tex) (read-comment current-folder))
                              (dolist (i widget-list)
                                (ltk:destroy i))
                              (setf widget-list nil)
                              (setf (ltk:text folder-label) (concat "Current folder: " (simplified-path current-folder)))
                              (let ((subfolders (subfolders current-folder)))
                                (multiple-value-bind (scores probsum pair-scores pair-words pair-word-scores) (scores vocab
                                                                                                                      subfolders
                                                                                                                      (map-to-hash #'get-recursive-corpus subfolders)
                                                                                                                      (map-to-hash #'get-word-count subfolders))
                                  (setf (ltk:text probsum-label) (concat "Maximum possible probability: " (my-round (/ (fallback probsum 1)))))
                                  (if (and subfolders *explain?*)
                                      (pair-scores-explainer 0 0 (window "HUJAJA") vocab subfolders pair-scores pair-words pair-word-scores))
                                  (let ((counter 1)
                                        (sorted-subfolders (sort (copy-seq subfolders) #'> :key #'(lambda (folder) (fallback (gethash folder scores) (/ (length subfolders)))))))
                                    (dolist (i sorted-subfolders)
                                     (incf counter)
                                     (push (button counter
                                                   0
                                                   folder-frame
                                                   (concat (file-name i t) " score: " (my-round (fallback (gethash i scores) (/ (length subfolders)))) ", " (get-word-count i) " words, " (get-file-count i) " files.")
                                                   #'(lambda ()
                                                       (redraw i)))
                                           widget-list)))))
                              (let ((links (class-links current-folder)))
                                (ltk:destroy file-list)
                                (setf file-list (scrollable-list 1 0 file-frame *entries-per-page* links
                                                                  (mapcar #'(lambda (link)
                                                                              #'(lambda () (link-options-window link (concat current-folder "links"))))
                                                                          links)))))

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
                     (label 0 0 folder-frame "FOLDERS")
                     (button 1 0 folder-frame ".." #'(lambda () (redraw (parent-folder current-folder))))

                     (label 0 0 file-frame "FILES")
                     (let ((links (class-links current-folder)))
                       (scrollable-list 1 0 file-frame *entries-per-page* links
                                        (mapcar #'(lambda (link)
                                                    #'(lambda () (link-options-window link (concat current-folder "links"))))
                                                links)))
                     
                     (label 1 0 options-frame (concat "Current URL: " url))
                     (button 3 0 options-frame "Create folder" #'(lambda () (let ((txt (ltk:text e)))
                                                                              (ensure-directories-exist (concat current-folder txt "/"))
                                                                              (overwrite-file (concat current-folder txt "/corpus") nil)
                                                                              (overwrite-file (concat current-folder txt "/file-count") 0)
                                                                              (overwrite-file (concat current-folder txt "/links") nil)
                                                                              (setf (ltk:text e) "")
                                                                              (redraw (concat current-folder txt "/")))))
                     (button 4 0 options-frame "Add link here" #'(lambda ()
                                                                   ;; Avoiding double insert
                                                                   (if (not (member url (class-links current-folder) :test #'equal))
                                                                       (let* ((links-file (concat current-folder "links"))
                                                                              (existing-links (read-from-file links-file)))
                                                                         (log-print (concat "Adding URL " url " to folder " (simplified-path current-folder)))
                                                                         (redownload-file url)
                                                                         (overwrite-file links-file (append1 existing-links url))
                                                                         (setf current-url "")
                                                                         (back-to-main))
                                                                       (log-print "File already in folder."))))
                     
                     (button 1 0 comment-frame "Save comment" #'(lambda ()
                                                                  (log-print "Saved comment in " (simplified-path current-folder))
                                                                  (set-comment (ltk:text tex) current-folder)))
                     (redraw-confirmed current-folder)
                     fr)))

               ;; TBD: This is mostly duplicate and TEMP.
               (word-explainer (r c master word)
                 (let* ((fr (frame r c master))
                        (current-folder *classes-folder*)
                        (counter 0)
                        (widget-list nil)
                        (f (frame 0 0 fr)) ; frame for everything except the comment
                        (comment-frame (frame 0 1 fr))
                        (tex (text 0 0 comment-frame "")))
                   (labels ((redraw (new-path)
                              (setf current-folder new-path)
                              (setf (ltk:text tex) (read-comment current-folder))
                              (dolist (i widget-list)
                                (ltk:destroy i))
                              (setf widget-list nil)
                              (setf counter 1)
                              (push (label 0 2 f (concat "Current folder: " current-folder))
                                    widget-list)
                              ;; produces conses of (subfolder . score)
                              (let* ((subfolders (subfolders current-folder)))
                                (dolist (i subfolders)
                                  (incf counter)
                                  (push (button counter
                                                1
                                                f
                                                (concat (file-name i t)
                                                        " word count: "
                                                        (occurrences word (get-recursive-corpus i))
                                                        ", out of "
                                                        (get-word-count i)
                                                        " words in total. Portion: "
                                                        (my-round (* 10000 (/ (occurrences word (get-recursive-corpus i))
                                                                              (get-word-count i))))
                                                        "‱")
                                                #'(lambda ()
                                                    (redraw i)))
                                        widget-list)))))
                     (label 0 1 f "FOLDERS")
                     (button 1 1 f ".." #'(lambda () (redraw (parent-folder current-folder))))
                     (label 1 2 f (concat "Current word: " word))
                     (redraw current-folder)
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
        (let ((log (frame 1 1 W))
              (log-list nil))
          (button 0 0 log "Wipe log" #'(lambda ()
                                         (dolist (button log-list)
                                           (ltk:destroy button))
                                         (setf log-list nil)))
          (defun log-print (&rest strings)
            ;; BUG: Log entry seems to show with a lag.
            (let ((full-string (apply #'concat strings)))
              (push (button (1+ (length log-list)) 0 log full-string #'pass) log-list)
              full-string)))
        (let ((X (frame 2 1 W)))
          (button 0 0 X "X" #'back-to-main))
        (back-to-main)))))
