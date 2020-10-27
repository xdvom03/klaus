#|
TBD: Make X close everything from any window.
TBD: Add the ability to put more links as one file. Generalise files. Then add SpaceX WBW post.
TBD: Control for page length bias?
TBD: Put the comment in a separate frame, so that its height does not mess up formatting.
BUG: If a comment file exists, but is empty (not even an empty string), an error is signalled.
TBD: Prevent all errors coming from empty or invalid links in the link entry.
TBD: Use structs instead of lists for multiple returns to prevent picking out the wrong return.
TBD: Use a pair-fight system with weights by eventual strength to decide between 3+ folders.

There's a problem that might require some single transferrable vote system or something like that.
The starting threeway decision tends to go like this:
We know it's not boilerplate because of a bunch of meaningful words.
There are a couple trash-y words, but that might get fixed as I add more data. Mostly errors.
It seems not to be valuable because I can find a couple boilerplate-y words. These tend to (maybe?) class more in trash than valuable.
We get tied trash and valuable, but the boilerplate-y words should not be considered at all because they can only be admitted as evidence for the boilerplate theory.
Check out if a system that weighs word occurrences by the scores of their corresponding categories converges to a solution.

What do we do with <noscript>? It caused c2wiki to class as boilerplate, but I think there's no good reason to exclude it. Made irrelevant by C2wiki depending on Javascript, and thus not being downloadable.
Related TBD: Redownload button.
|#

(defparameter *entries-per-page* 20)
(defparameter *try-to-class?* t)
(defparameter *explain?* t)
(defparameter *evidence-length* 6)

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
  ;; In final folders, we don't want to do anything
  (if (and on? (subfolders folder) *try-to-class?*)
      (let* (;; (path file-count corpus score)
             (subfolder-count (length (subfolders folder)))
             (default-score (/ subfolder-count))
             (subfolders-data (mapcar #'(lambda (path)
                                          (list path (get-file-count path) (get-recursive-corpus path) default-score))
                                      (subfolders folder)))
             (smallest-subfolder (apply #'min (mapcar #'second subfolders-data)))
             (total-corpus (get-subfolder-corpus folder))
             ;; Remove totally unknown words (TBD: Check if this is actually needed)
             (vocab (remove-if #'(lambda (word) (= (occurrences word total-corpus) 0))
                               (gethash nil (wordlist (url-text url)))))
             ;; (path corpus score)
             (normalised-subfolders (mapcar #'(lambda (data)
                                                (list (first data)
                                                      (normalize-corpus (get-recursive-corpus (first data))
                                                                        (/ smallest-subfolder (get-file-count (first data))) ; TBD: Warn if a folder is empty!
                                                                        vocab)
                                                      default-score))
                                            subfolders-data))
             (total-normalised-corpus (reduce #'add-hashtable-corpuses (mapcar #'second normalised-subfolders)))
             (explainer-data nil))
        
        (dolist (subfolder normalised-subfolders)
          ;; balance evidence for and against
          (let ((scores (cons (simplified-path (first subfolder))
                              (append (subseq (sort (copy-seq (mapcar #'(lambda (word) (list (coerce (word-probability (occurrences word (second subfolder))
                                                                                                                       (occurrences word total-normalised-corpus)
                                                                                                                       subfolder-count)
                                                                                                     'single-float)
                                                                                             word
                                                                                             (coerce (occurrences word (second subfolder)) 'single-float)
                                                                                             (coerce (occurrences word total-normalised-corpus) 'single-float)))
                                                                      vocab))
                                                    #'> :key #'car)
                                              0
                                              (min *evidence-length* (length vocab)))
                                      (reverse (subseq (sort (copy-seq (mapcar #'(lambda (word) (list (coerce (word-probability (occurrences word (second subfolder))
                                                                                                                                (occurrences word total-normalised-corpus)
                                                                                                                                subfolder-count)
                                                                                                              'single-float)
                                                                                                      word
                                                                                                      (coerce (occurrences word (second subfolder)) 'single-float)
                                                                                                      (coerce (occurrences word total-normalised-corpus) 'single-float)))
                                                                               vocab))
                                                             #'< :key #'car)
                                                       0
                                                       (min *evidence-length* (length vocab))))))))
            (setf (third subfolder) (apply #'* (mapcar #'car (cdr scores))))
            (push scores explainer-data)))
        (let ((prob-sum (apply #'+ (mapcar #'third normalised-subfolders))))
          (dolist (subfolder normalised-subfolders)
            (setf (third subfolder)
                  (/ (third subfolder)
                     prob-sum))))
        (if *explain?*
            (scores-explainer (reverse explainer-data)))
        (mapcar #'(lambda (a) (cons (first a)
                                    (coerce (third a) 'single-float)))
                normalised-subfolders))
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
    (labels ((redraw ()
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
                                       (setf current-folder subfolder)
                                       (redraw)))
                           widget-list))))))
      (button 0 0 f "X" #'(lambda () (ltk:destroy W)))
      (label 0 1 f "FOLDERS")
      (button 3 2 f "Create folder" #'(lambda () (let ((txt (ltk:text e))
                                                       (subfolders-folder (concat current-folder "subfolders/")))
                                                   (ensure-directories-exist (concat subfolders-folder txt "/analysis/"))
                                                   (create-file (concat subfolders-folder txt "/analysis/corpus") nil)
                                                   (create-file (concat subfolders-folder txt "/analysis/file-count") 0)
                                                   (ensure-directories-exist (concat subfolders-folder txt "/files/"))
                                                   (ensure-directories-exist (concat subfolders-folder txt "/subfolders/"))
                                                   (setf (ltk:text e) "")
                                                   (setf current-folder (concat subfolders-folder txt "/"))
                                                   (redraw))))
      (button 1 1 f ".." #'(lambda () (setf current-folder (parent-folder (parent-folder current-folder)))
                             (redraw)))
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
      (redraw))))

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
        ;; BUG: Log entry seems to show with a lag.)
        (let ((full-string (apply #'concat (mapcar #'(lambda (a) (if (stringp a)
                                                                     a
                                                                     (write-to-string a)))
                                                   strings))))
          (push (button (length log-list) 0 LW full-string #'pass) log-list)
          full-string))
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
      (button 0 0 W "X" #'(lambda () (ltk:destroy W) (ltk:destroy ltk:*tk*)))
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
