(defun words-explainer (r c master class-1 class-2 words1 words2 word-details-1 word-details-2 page-length)
  ;; words still have the vocab format
  (let ((f (frame r c master))
        ;; The reversion stems from chosen-words adding words to the chosen acc from the strongest evidence to weakest, which causes strong evidence to end up at the end of the key list
        (individual-words-1 (reverse (list-keys words1)))
        (individual-words-2 (reverse (list-keys words2))))
    (button 0 0 f class-2 #'pass)
    (button 0 1 f class-1 #'pass)
    (scrollable-list 1 0 f page-length (mapcar #'list
                                               individual-words-1
                                               (mapcar #'(lambda (word) (gethash word word-details-1)) individual-words-1)))
    (scrollable-list 1 1 f page-length (mapcar #'list
                                               individual-words-2
                                               (mapcar #'(lambda (word) (gethash word word-details-2)) individual-words-2)))))

(defun pair-scores-explainer (r c master classes pair-scores pair-words pair-word-details)
  (let* ((f (frame r c master)))
    (dotimes (i (length classes))
      (let ((class (nth i classes)))
        (button 0 (1+ i) f (folder-name class) #'pass)
        (button (1+ i) 0 f (folder-name class) #'pass)
        (let* ((total-score (apply #'+ (mapcar #'(lambda (opponent)
                                                   (if (equal class opponent)
                                                       0
                                                       (gethash (cons class opponent) pair-scores)))
                                               classes))))
          (button (1+ i)
                  (1+ (length classes))
                  f
                  (write-to-string (my-round total-score))
                  #'pass))
        
        (dotimes (j (length classes))
          (let* ((opponent (nth j classes))
                 (pair-1 (cons class opponent))
                 (chosen-words-1 (gethash pair-1 pair-words))
                 (word-details-1 (gethash pair-1 pair-word-details))
                 (pair-2 (cons opponent class))
                 (chosen-words-2 (gethash pair-2 pair-words))
                 (word-details-2 (gethash pair-2 pair-word-details))
                 (score (gethash pair-1 pair-scores)))
            (button (1+ i)
                    (1+ j)
                    f
                    (if (= i j)
                        ""
                        (write-to-string (my-round score)))
                    (if (= i j)
                        #'pass
                        #'(lambda () (words-explainer 0 0
                                                      (window "Words explainer")
                                                      (folder-name class)
                                                      (folder-name opponent)
                                                      chosen-words-1 chosen-words-2
                                                      word-details-1 word-details-2
                                                      *entries-per-page*))))))))
    f))

(let ((explain? nil))
  (defun database-window (r c master vocab)
    (let* ((fr (frame r c master))
           (current-class "/")
           (widget-list nil)
           
           (class-frame (frame 0 0 fr))
           (comment-frame (frame 0 2 fr))

           ;; variable stuff
           (tex (text 0 0 comment-frame "" 10 20))
           (class-label (label 0 0 class-frame ""))
           parent-button)
      (labels ((redraw (new-path)
                 (setf current-class new-path)
                 (if (equal new-path "/")
                     (ltk:configure parent-button :state :disabled)
                     (ltk:configure parent-button :state :normal))
                 (setf (ltk:text tex) (read-comment current-class))
                 (dolist (i widget-list)
                   (ltk:destroy i))
                 (setf widget-list nil)
                 (setf (ltk:text class-label) current-class)
                 (let ((subclasses (classifier-options current-class)))
                   (multiple-value-bind (scores probsum pair-scores pair-words pair-word-details)
                       (scores vocab
                               subclasses
                               (map-to-hash #'get-recursive-corpus subclasses)
                               (map-to-hash #'get-word-count subclasses))
                     (declare (ignore probsum))
                     (if (and (> (length subclasses) 1) explain?)
                         (pair-scores-explainer 0 0 (window "Table explainer") subclasses pair-scores pair-words pair-word-details))
                     (let ((counter 1)
                           (sorted-subclasses (sort (copy-seq subclasses) #'> :key #'(lambda (class) (fallback (gethash class scores) (/ (length subclasses)))))))
                       (dolist (i sorted-subclasses)
                         (incf counter)
                         (push (button counter
                                       0
                                       class-frame
                                       (concat (folder-name i) " score: " (my-round (fallback (gethash i scores) (/ (length subclasses)))))
                                       #'(lambda ()
                                           (redraw i)))
                               widget-list)))))))
        
        (setf parent-button (button 1 0 class-frame ".." #'(lambda () (redraw (parent-class current-class)))))
        (redraw current-class)
        fr)))
  
  (defun classifier-window ()
    (let* ((W (window "Classifier"))
           (displayed-frame nil)
           (current-url ""))
      (labels ((change-screen (new-frame)
                 (if displayed-frame
                     (ltk:destroy displayed-frame))
                 (setf displayed-frame new-frame))
             
               (main-menu (r c master)
                 (letrec ((fr (frame r c master))
                          (e1 (entry 0 0 fr))
                          (t1 (text 1 0 fr "" 20 20))
                          (ch (checkbox 3 0 fr "Explain classing?" #'(lambda ()
                                                                       (setf explain? (ltk:value ch))))))
                   (choose-file-button 2 0 fr "Load text from file" t1)
                   (setf (ltk:text e1) current-url)
                   (button 0 1 fr "Open database with URL" #'(lambda ()
                                                               (setf current-url (ltk:text e1))
                                                               (ltk:wm-title W current-url)
                                                               (handler-case (let* ((vocab (tokens (url-text current-url)))
                                                                                    (word-count (word-count vocab)))
                                                                               (if (< word-count
                                                                                      *min-word-count*)
                                                                                   (warning-box (concat "This file is very short. Word count: " word-count ". Does that seem right?") "Site text very short."))
                                                                               (change-screen (database-window 0 1 master vocab)))
                                                                 (error (err-text)
                                                                   (warning-box err-text "Website error")
                                                                   (back-to-main)))))
                   (button 0 2 fr "Place URL" #'(lambda ()
                                                  (warn-on-error ("Website error")
                                                    (info-box (place (ltk:text e1)) (ltk:text e1)))))
                   (button 1 1 fr "Open database with text"
                           #'(lambda ()
                               (setf current-url (ltk:text e1))
                               (ltk:wm-title W current-url)
                               (change-screen (database-window 0 1 master (tokens (extract-text (ltk:text t1)))))))
                   (button 1 2 fr "Place text" #'(lambda ()
                                                   (info-box (place-vocab (tokens (extract-text (ltk:text t1)))) "Placing manual input")))
                   (setf (ltk:value ch) explain?)
                   fr))
             
               (back-to-main ()
                 (change-screen (main-menu 0 1 W))))
        (let ((X (frame 2 1 W)))
          (button 0 0 X "Back to classifier menu" #'(lambda ()
                                                      (back-to-main)))
          (button 1 0 X "Go to database" #'(lambda ()
                                             (ltk:destroy W)
                                             (db-window)))
          (button 2 0 X "Go to crawler" #'(lambda ()
                                            (ltk:destroy W)
                                            (crawler-window))))
        (back-to-main)
        (ltk:on-close W #'(lambda () (ltk:destroy ltk:*tk*)))
        W))))

(defun run ()
  (ltk:with-ltk ()
    (ltk:withdraw ltk:*tk*)
    (classifier-window)))
