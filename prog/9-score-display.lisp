(defun words-explainer (r c master words word-details page-length)
  ;; TBD: Move the exps to a more reasonable place
  (let* ((f (frame r c master)))
    (button 0 0 f "words for" #'pass)
    (button 0 1 f "words against" #'pass)
    (scrollable-list 1 0 f page-length (mapcar #'list
                                               words
                                               (mapcar #'(lambda (word) (gethash word word-details)) words)))))

(defun pair-scores-explainer (r c master classes pair-scores pair-words pair-word-details) ; TBD: Fix names
  ;; The scores FOR a given class are in its rows. TBD: Make that clear from the window
  (let* ((f (frame r c master)))
    (dotimes (i (length classes))
      (let ((class (nth i classes)))
        (button 0 (1+ i) f (folder-name class) #'pass)
        (button (1+ i) 0 f (folder-name class) #'pass)
        (let* ((total-score (apply #'+ (mapcar #'(lambda (opponent)
                                                   (if (equal class opponent)
                                                       0
                                                       (gethash (cons class opponent) pair-scores)))
                                               classes)))
               (total-score-button (button (1+ i)
                                           (1+ (length classes))
                                           f
                                           (write-to-string (my-round total-score))
                                           #'pass)))
          (ltk:configure total-score-button :background (color-code total-score)))
        (dotimes (j (length classes))
          (let* ((opponent (nth j classes))
                 (pair (cons class opponent))
                 (chosen-words (gethash pair pair-words))
                 (word-details (gethash pair pair-word-details))
                 (score (gethash pair pair-scores))
                 (b (button (1+ i)
                            (1+ j)
                            f
                            (if (= i j)
                                ""
                                (write-to-string (my-round score)))
                            (if (= i j)
                                #'pass
                                #'(lambda () (words-explainer 100 100
                                                              (window (concat (folder-name class)
                                                                              " over "
                                                                              (folder-name opponent)))
                                                              chosen-words word-details *entries-per-page*))))))
            (if score (ltk:configure b :background (color-code score)))))))
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
        ((> score (ln 0.5)) "#780")     ; 0 to 0.5 sigma
        ((> score (ln 0.30854)) "#870") ; -0.5 to 0
        ((> score (ln 0.15866)) "#960") ; -1 to -0.5 sigma
        ((> score (ln 0.06681)) "#a50") ; -1.5 to -1
        ((> score (ln 0.02275)) "#b40") ; -2 to -1.5 sigma
        ((> score (ln 0.00621)) "#c30") ; -2.5 to -2
        ((> score (ln 0.00135)) "#d20") ; -3 to -2.5 sigma
        ((> score (ln 0.00023)) "#e10") ; -3.5 to -3
        (t "#f00")                      ; under -3.5 sigma
        ))

(defun database-window (r c master vocab explain?)
  (let* ((fr (frame r c master))
         (current-class "/")
         (widget-list nil)
                        
         (class-frame (frame 0 0 fr))
         (comment-frame (frame 0 2 fr))

         ;; variable stuff
         (tex (text 0 0 comment-frame "" 10 20 "NotoSans 10"))
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
               (let ((subclasses (subclasses current-class)))
                 (multiple-value-bind (scores probsum pair-scores pair-words pair-word-details)
                     (scores vocab
                             subclasses
                             (map-to-hash #'get-recursive-corpus subclasses)
                             (map-to-hash #'get-word-count subclasses))
                   (declare (ignore probsum))
                   (if (and (> (length subclasses) 1) explain?)
                       ;; TBD: Instead of scores, provide more details!
                       (pair-scores-explainer 0 0 (window "HUJAJA") subclasses pair-scores pair-words pair-word-details))
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

(let ((explain? nil))
  (defun database-window (r c master vocab)
    (let* ((fr (frame r c master))
           (current-class "/")
           (widget-list nil)
           
           (class-frame (frame 0 0 fr))
           (comment-frame (frame 0 2 fr))

           ;; variable stuff
           (tex (text 0 0 comment-frame "" 10 20 "NotoSans 10"))
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
                 (let ((subclasses (subclasses current-class)))
                   (multiple-value-bind (scores probsum pair-scores pair-words pair-word-details)
                       (scores vocab
                               subclasses
                               (map-to-hash #'get-recursive-corpus subclasses)
                               (map-to-hash #'get-word-count subclasses))
                     (declare (ignore probsum))
                     (if (and (> (length subclasses) 1) explain?)
                         ;; TBD: Instead of scores, provide more details!
                         (pair-scores-explainer 0 0 (window "HUJAJA") subclasses pair-scores pair-words pair-word-details))
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
    (let* ((W (window "klaus"))
           (displayed-frame nil)
           (current-url ""))
      (labels ((change-screen (new-frame)
                 (if displayed-frame
                     (ltk:destroy displayed-frame))
                 (setf displayed-frame new-frame))
             
               (main-menu (r c master)
                 (letrec ((fr (frame r c master))
                          (e1 (entry 0 0 fr))
                          (t1 (text 1 0 fr "" 20 20 "NotoSans 10"))
                          (ch (checkbox 0 2 fr "Explain classing?" #'(lambda (a)
                                                                       a
                                                                       (setf explain? (ltk:value ch))
                                                                       (setf (ltk:value ch) explain?)))))
                   (setf (ltk:text e1) current-url)
                   (button 0 1 fr "Open database" #'(lambda ()
                                                      (setf current-url (ltk:text e1))
                                                      (ltk:wm-title W current-url)
                                                      (handler-case (change-screen (database-window 0 1 master (hashtable-to-count-list ;;remove-duplicates
                                                                                                                (tokens (url-text current-url))
                                                                                                                )))
                                                        (error (err-text)
                                                          (warning-box err-text "Website error")
                                                          (back-to-main)
                                                          (abort)))))
                   (button 1 1 fr "Open database with text"
                           #'(lambda ()
                               (setf current-url (ltk:text e1))
                               (ltk:wm-title W current-url)
                               (change-screen (database-window 0 1 master (remove-duplicates (wordlist (clean-text (ltk:text t1))))))))
                   (setf (ltk:value ch) explain?)
                   fr))
             
               (back-to-main ()
                 (change-screen (main-menu 0 1 W))))
        (let ((X (frame 2 1 W)))
          (button 0 0 X "X" #'(lambda ()
                                (back-to-main)))
          (button 1 0 X "Go to database" #'(lambda ()
                                             (ltk:destroy W)
                                             (db-window))))
        (back-to-main)
        (ltk:on-close W #'(lambda () (ltk:destroy ltk:*tk*)))
        W))))

(defun run ()
  (ltk:with-ltk ()
    (ltk:withdraw ltk:*tk*)
    (classifier-window)))

(defun hashtable-to-count-list (hashtable)
  ;; TEMP: This is a quick hack to consider text volume and not just a list of keywords - do it in a math-normal multiplying way!
  (let ((corpus nil))
    (dolist (word (list-keys hashtable))
      (push (make-list (gethash word hashtable) :initial-element word) corpus))
    (reduce #'append corpus)))
