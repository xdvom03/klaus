#|
TBD: Use a pair-fight system with weights by eventual strength to decide between 3+ folders.
TBD: Display bottlenecks for precision (subfolder with fewest files).

Proof against tends to be a stronger discriminator than proof for. Is that a problem? It helps distinguish, say, articles about copyright from legal boilerplate. But what about the general case?

There's a problem that might require some single transferrable vote system or something like that.
The starting threeway decision tends to go like this:
We know it's not boilerplate because of a bunch of meaningful words.
There are a couple trash-y words, but that might get fixed as I add more data. Mostly errors.
It seems not to be valuable because I can find a couple boilerplate-y words. These tend to (maybe?) class more in trash than valuable.
We get tied trash and valuable, but the boilerplate-y words should not be considered at all because they can only be admitted as evidence for the boilerplate theory.
Check out if a system that weighs word occurrences by the scores of their corresponding categories converges to a solution.

What do we do with <noscript>? It caused c2wiki to class as boilerplate, but I think there's no good reason to exclude it. Made irrelevant by C2wiki depending on Javascript, and thus not being downloadable.
Related TBD: Redownload button.
TBD: Reword corpus rebuilding using hash tables.
|#

(defparameter *entries-per-page* 20)
(defparameter *try-to-class?* t)
(defparameter *explain?* t)
(defparameter *evidence-length* 6)
(defparameter *newline* "
")

(defun word-probability (target total subfolder-count)
  (/ (+ 1 target)
     (+ subfolder-count total)))

(defun scores-explainer (data)
  ;; data is a list of lists. Each list is a subfolder path and a bunch of pieces of evidence. Each of these occupies a separate button. Also show where the score came from!
  (let* ((W (window "explaining scores since 17-10-2020")))
    (dotimes (i (length data))
      (let ((path (car (nth i data)))
            (evidence (cdr (nth i data))))
        (button 0 (1+ i) W path #'pass)
        (dotimes (j (length evidence))
          (button (1+ j) (1+ i) W (nth j evidence) #'pass))))
    W))

(defun evaluate (url folder &optional (on? t))
  ;; Has to return a list of conses (subfolder . score)
  ;; In folders without subfolders, we don't want to do anything
  (if (and on? (subfolders folder) *try-to-class?*)
      (let* ((subfolder-count (length (subfolders folder)))
             (subfolder-paths (subfolders folder))
             (smallest-subfolder (apply #'min (mapcar #'get-file-count subfolder-paths)))
             ;; Remove totally unknown words (TBD: Check if this is actually needed)
             (vocab (remove-if #'(lambda (word) (= (occurrences word total-corpus) 0))
                               (gethash nil (wordlist (url-text url)))))
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

(defun database-window (url-entry)
  (let* ((url (ltk:text url-entry))
         (W (make-instance 'ltk:toplevel :title "DATABASE"))
         (current-folder *home-folder*)
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
               (let ((scores (evaluate url current-folder)))
                 (dolist (i scores)
                   (let ((subfolder (car i)))
                     (incf counter)
                     (push (button counter
                                   1
                                   f
                                   (concat (file-name subfolder t) (write-to-string (cdr i)))
                                   #'(lambda ()
                                       (redraw subfolder)))
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
      (button 0 0 f "X" #'kill-all)
      (label 0 1 f "FOLDERS")
      (button 3 2 f "Create folder" #'(lambda () (let ((txt (ltk:text e))
                                                       (subfolders-folder (concat current-folder "subfolders/")))
                                                   (ensure-directories-exist (concat subfolders-folder txt "/analysis/"))
                                                   (create-file (concat subfolders-folder txt "/analysis/corpus") nil)
                                                   (create-file (concat subfolders-folder txt "/analysis/file-count") 0)
                                                   (ensure-directories-exist (concat subfolders-folder txt "/files/"))
                                                   (ensure-directories-exist (concat subfolders-folder txt "/subfolders/"))
                                                   (setf (ltk:text e) "")
                                                   (redraw (concat subfolders-folder txt "/")))))
      (button 1 1 f ".." #'(lambda () (redraw (parent-folder (parent-folder current-folder)))))
      (label 1 2 f (concat "Current URL: " url))
      (button 4 2 f "Add link here" #'(lambda ()
                                        ;; Avoiding double insert
                                        (if (not (member url (file-urls current-folder) :test #'equal))
                                            (progn
                                              (log-print (concat "Adding URL " url " to folder " (simplified-path current-folder)))
                                              (let* ((files-folder (concat current-folder "files/"))
                                                     (file-count (length (directory* files-folder)))
                                                     (new-file-folder (concat files-folder (write-to-string (1+ file-count)) "/")))
                                                (ensure-directories-exist new-file-folder)
                                                (log-print (concat "Downloading URL: " url))
                                                (with-open-file (stream (concat new-file-folder "url") :direction :output :if-exists :append :if-does-not-exist :create)
                                                  (print url stream))
                                                (with-open-file (stream (concat new-file-folder "html") :direction :output :if-exists :append :if-does-not-exist :create)
                                                  (print (safe-fetch-html url) stream))
                                                (with-open-file (stream (concat new-file-folder "text") :direction :output :if-exists :append :if-does-not-exist :create)
                                                  (print (url-text url) stream))
                                                (log-print "Download complete."))
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
         (url (file-url folder)))
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
        (let ((full-string (apply #'concat (mapcar #'(lambda (a) (if (stringp a)
                                                                     a
                                                                     (write-to-string a)))
                                                   strings))))
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
      (button 0 2 W "Open & edit history" #'(lambda () (history-window e1)))
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
