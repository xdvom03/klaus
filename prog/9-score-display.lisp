(defun words-explainer (r c master words word-details page-length)
  ;; TBD: Move the exps to a more reasonable place
  (let* ((f (frame r c master)))
    (button 0 0 f "words for" #'pass)
    (button 0 1 f "words against" #'pass)
    (scrollable-list 1 0 f page-length (mapcar #'list
                                               words
                                               (mapcar #'(lambda (word) (gethash word word-details)) words)))))

(defun pair-scores-explainer (r c master folders pair-scores pair-words pair-word-details) ; TBD: Fix names
  ;; The scores FOR a given folder are in its rows. TBD: Make that clear from the window
  (let* ((f (frame r c master)))
    (dotimes (i (length folders))
      (let ((folder (nth i folders)))
        (button 0 (1+ i) f (folder-name folder) #'pass)
        (button (1+ i) 0 f (folder-name folder) #'pass)
        (button (1+ i)
                (1+ (length folders))
                f
                (write-to-string (my-round (apply #'+ (mapcar #'(lambda (opponent)
                                                                  (if (equal folder opponent)
                                                                      0
                                                                      (gethash (cons folder opponent) pair-scores)))
                                                              folders))))
                #'pass)
        (dotimes (j (length folders))
          (let* ((opponent (nth j folders))
                 (pair (cons folder opponent))
                 (chosen-words (gethash pair pair-words))
                 (word-details (gethash pair pair-word-details))
                 (score (gethash pair pair-scores))
                 (b (button (1+ i)
                            (1+ j)
                            f
                            (if (= i j)
                                ""
                                (write-to-string (my-round score)))
                            #'(lambda () (words-explainer 100 100 (window (concat (simplified-path folder) " over " (simplified-path opponent))) chosen-words word-details *entries-per-page*)))))
            (if (not (integerp score)) ;; integer is an empty score
                (ltk:configure b :background (color-code score)))))))
    f))

(defun color-code (score)
  ;; divided by standard deviations, coded in because the error function would require a separate library
  (cond ((> score (ln 0.99977)) "#0f0") ; over 3.5
        ((> score (ln 0.99865)) "#1e0") ; 3 to 3.5
        ((> score (ln 0.99379)) "#2d0") ; 2.5 to 3
        ((> score (ln 0.97725)) "#3c0") ; 2 to 2.5 sigma
        ((> score (ln 0.93319)) "#4b0") ; 1.5 to 2
        ((> score (ln 0.84134)) "#5a0") ; 1 to 1.5 sigma
        ((> score (ln 0.69146)) "#690") ; 0.5 to 1
        ((> score (ln 0.5)) "#780") ; 0 to 0.5 sigma
        ((> score (ln 0.30854)) "#870") ; -0.5 to 0
        ((> score (ln 0.15866)) "#960") ; -1 to -0.5 sigma
        ((> score (ln 0.06681)) "#a50") ; -1.5 to -1
        ((> score (ln 0.02275)) "#b40") ; -2 to -1.5 sigma
        ((> score (ln 0.00621)) "#c30") ; -2.5 to -2
        ((> score (ln 0.00135)) "#d20") ; -3 to -2.5 sigma
        ((> score (ln 0.00023)) "#e10") ; -3.5 to -3
        (t "#f00") ; under -3.5 sigma
        ))

(defun run ()
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
               
               (main-menu (r c master)
                 (letrec ((W (frame r c master))
                          (e1 (entry 0 0 W))
                          (ch (checkbox 0 2 W "Explain classing?" #'(lambda (a)
                                                                      a
                                                                      (setf *explain?* (ltk:value ch))
                                                                      (setf (ltk:value ch) *explain?*))))
                          (ch2 (checkbox 0 3 W "Blind?" #'(lambda (a)
                                                            a
                                                            (setf *blind?* (ltk:value ch2))
                                                            (setf (ltk:value ch2) *blind?*)))))
                   (setf (ltk:text e1) current-url)
                   (button 0 1 W "Open database" #'(lambda ()
                                                     (setf current-url (ltk:text e1))
                                                     (change-screen (database-window 0 1 master current-url))))
                   (setf (ltk:value ch) *explain?*)
                   (setf (ltk:value ch2) *blind?*)
                   W))
               
               (database-window (r c master url)
                 (let* ((fr (frame r c master))
                        (current-folder *classes-folder*)
                        (widget-list nil)
                        
                        (folder-frame (frame 0 0 fr))
                        (options-frame (frame 0 1 fr))
                        (comment-frame (frame 0 2 fr))

                        (vocab (remove-duplicates (tokens (url-text url)) :test #'equal))

                        ;; variable stuff
                        (tex (text 0 0 comment-frame "" 10 20 "NotoSans 10"))
                        (folder-label (label 0 0 options-frame ""))
                        parent-button)
                   (labels ((redraw (new-path)
                              (setf current-folder new-path)
                              (if (equal (simplified-path new-path) (simplified-path *classes-folder*))
                                  (ltk:configure parent-button :state :disabled)
                                  (ltk:configure parent-button :state :normal))
                              (setf (ltk:text tex) (read-comment current-folder))
                              (dolist (i widget-list)
                                (ltk:destroy i))
                              (setf widget-list nil)
                              (setf (ltk:text folder-label) (concat "Current folder: " (simplified-path current-folder)))
                              (let ((subfolders (subfolders current-folder))
                                    (excluded-data (mapcar #'(lambda (folder) (cons current-url folder))
                                                           (link-occurrences current-url))))
                                (multiple-value-bind (scores probsum pair-scores pair-words pair-word-details)
                                    (scores vocab
                                            subfolders
                                            (if *blind?*
                                                (map-to-hash #'(lambda (folder)
                                                                 (reduce #'add-hashtable-corpuses
                                                                         (append1 (remove-if #'null
                                                                                             (mapcar #'(lambda (excludee)
                                                                                                         (if (equal (cdr excludee) folder)
                                                                                                             (scale-corpus (downloaded-link-corpus (car excludee)) -1)))
                                                                                                     excluded-data))
                                                                                  (get-recursive-corpus folder))))
                                                             subfolders)
                                                (map-to-hash #'get-recursive-corpus subfolders))
                                            (if *blind?*
                                                (map-to-hash #'(lambda (folder)
                                                                 (apply #'+
                                                                        (append1 (remove-if #'null
                                                                                            (mapcar #'(lambda (excludee)
                                                                                                        (if (equal (cdr excludee) folder)
                                                                                                            (- (length (list-keys (downloaded-link-corpus (car excludee)))))))
                                                                                                    excluded-data))
                                                                                 (get-word-count folder))))
                                                             subfolders)
                                                (map-to-hash #'get-word-count subfolders)))
                                  (if (and (> (length subfolders) 1) *explain?*)
                                      ;; TBD: Instead of scores, provide more details!
                                      (pair-scores-explainer 0 0 (window "HUJAJA") subfolders pair-scores pair-words pair-word-details))
                                  (let ((counter 1)
                                        (sorted-subfolders (sort (copy-seq subfolders) #'> :key #'(lambda (folder) (fallback (gethash folder scores) (/ (length subfolders)))))))
                                    (dolist (i sorted-subfolders)
                                      (incf counter)
                                      (push (button counter
                                                    0
                                                    folder-frame
                                                    (concat (file-name i t) " score: " (my-round (fallback (gethash i scores) (/ (length subfolders)))))
                                                    #'(lambda ()
                                                        (redraw i)))
                                            widget-list)))))))
                     (label 0 0 folder-frame "FOLDERS")                     
                     (label 1 0 options-frame (concat "Current URL: " url))
                     
                     (setf parent-button (button 1 0 folder-frame ".." #'(lambda () (redraw (parent-folder current-folder)))))
                     (redraw current-folder)
                     fr)))
               
               (back-to-main ()
                 (change-screen (main-menu 0 1 W))))
        (let ((X (frame 2 1 W)))
          (button 0 0 X "X" #'(lambda ()
                                (back-to-main))))
        (back-to-main)))))

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
