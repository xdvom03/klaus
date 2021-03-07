#|
TBD: Load title for urls
TBD: Autoclose explainers when a thing is classed.
TBD: Remove a file from the downloaded stuffs when removed

Some sites use scripts to deliver boilerplate. While this is not a problem for classification (just ignore them or input manually), it might mess up the crawler.

Naming convention: 'class' is simplified path, 'folder' is actual folder.
|#

;;; NORMAL STUFFS
;;;----------------------------------------------------------------------------------------------
;;; GUI

(defparameter *modes-cycle* (list 'add 'remove 'move 'nothing))

(defun next-mode (mode)
  (fallback (second (member mode *modes-cycle* :test #'equal))
            (first *modes-cycle*)))

(defun action (url origin class mode)
  (case mode
    (add
     (add-url url class))
    (move
     ;; needlessly checks for continued existence of file
     (remove-url url origin)
     (add-url url class))
    (remove
     (remove-url url origin))
    (nothing
     nil)))

(defun db ()
  #|
  TBD: Abstract all the GUI stuff elsewhere
  TBD: This is a mess. Read up on C2 Wiki GUI design, see if this can be done more functionally.
  |#
  (ltk:with-ltk ()
    (ltk:withdraw ltk:*tk*)
    (let* ((W (window "klaus")))
      (ltk:on-close W #'(lambda () (ltk:destroy ltk:*tk*)))
      (let* ((fr (frame 0 0 W))
             (current-class "/")
             ;; Conses: (url . original-path)
             (bucket-urls nil)
             (subclass-buttons nil)
             (bucket-buttons nil)
             
             (class-frame (frame 0 0 fr))
             (file-frame (frame 1 0 W))
             (properties-frame (frame 0 1 file-frame))
             (comment-frame (frame 0 1 fr))
             (bucket-frame (frame 0 2 fr))
             (options-frame (frame 0 3 fr))

             ;; variable stuff
             (old-comment (read-comment current-class))
             (e (entry 1 0 bucket-frame))
             (tex (text 0 0 comment-frame "" 15 35 "NotoMono 10"))
             (file-list (frame 0 1 file-frame))
             parent-button

             ;; settings
             (mode (first *modes-cycle*))
             (show-file-counts? nil)
             (show-word-counts? nil)
             (show-word-details? nil)
             (show-files? t)

             (detail-frame (frame 5 0 options-frame))
             (e2 (entry 0 1 detail-frame))
             (files-label (if show-files? (label 0 0 file-frame "FILES")))
             (weight-entry (entry 0 0 properties-frame)))
        (labels ((class-description (class word)
                   (concat (folder-name class)
                           (if (or show-file-counts?
                                   show-word-counts?)
                               ": "
                               "")
                           (if show-word-counts?
                               (concat (my-round (* (get-weight class)
                                                    (get-word-count class)))
                                       " words ")
                               "")
                           (if show-file-counts?
                               (concat (get-file-count class) " files ")
                               "")
                           (if show-word-details?
                               (concat " word count: "
                                       (my-round (occurrences word (get-recursive-corpus class)))
                                       ", out of "
                                       (my-round (get-word-count class))
                                       " words in total. Portion: "
                                       (my-round (* 10000 (/ (occurrences word (get-recursive-corpus class))
                                                             (get-word-count class))))
                                       "‱")
                               "")))

                 (destroy-widgets (list)
                   (dolist (w list)
                     (ltk:destroy w)))

                 (new-bucket-button (url origin)
                   (let ((index (length bucket-buttons)))
                     (button (+ 3 index)
                             0
                             bucket-frame
                             (cons url origin)
                             #'(lambda ()
                                 (handler-case (action url origin current-class mode)
                                   (error (err-text)
                                     (warning-box err-text "Website error")
                                     (abort)))
                                 (setf bucket-urls (remove-nth index bucket-urls))
                                 (redraw-bucket)
                                 (redraw-files)))))
                 
                 (add-to-bucket (url origin)
                   (setf bucket-urls (append1 bucket-urls (cons url origin)))
                   (push (new-bucket-button url origin)
                         bucket-buttons))

                 (change-class-confirmed (new-path)
                   (setf current-class new-path)
                   (setf old-comment (read-comment new-path))
                   (ltk:configure parent-button :state (if (equal new-path
                                                                  "/")
                                                           :disabled
                                                           :normal))
                   (setf (ltk:text tex) (read-comment current-class))
                   (ltk:wm-title W current-class)
                   (redraw-subclasses)
                   (redraw-files))

                 (change-current-class (new-path)
                   (if (equal old-comment
                              (read-text-widget tex))
                       (change-class-confirmed new-path)
                       (let ((warning-button nil))
                         (setf warning-button (button 2 0 comment-frame "Change class despite unsaved comment" #'(lambda ()
                                                                                                                   (change-class-confirmed new-path)
                                                                                                                   (ltk:destroy warning-button)))))))

                 (redraw-subclasses ()
                   (setf (ltk:text weight-entry) (get-weight current-class))
                   (button 1 0 properties-frame "Set weight" #'(lambda () (set-weight current-class (read-from-string (ltk:text weight-entry)))))
                   (destroy-widgets subclass-buttons)
                   (setf subclass-buttons nil)
                   (let ((subclasses (subclasses current-class)))
                     (let ((counter 1))
                       (dolist (i subclasses)
                         (incf counter)
                         (push (button counter
                                       0
                                       class-frame
                                       (class-description i (intern (ltk:text e2)))
                                       #'(lambda ()
                                           (change-current-class i)))
                               subclass-buttons)))))

                 (redraw-files ()
                   (let ((urls (class-urls current-class)))
                     (ltk:destroy file-list)
                     (if show-files?
                         (setf file-list
                               (scrollable-list 1 0 file-frame *entries-per-page* urls (mapcar #'(lambda (url) (lambda () (add-to-bucket url current-class))) urls))))))
                 
                 (redraw-bucket ()
                   (destroy-widgets bucket-buttons)
                   (setf bucket-buttons nil)
                   (dolist (url bucket-urls)
                     (push (new-bucket-button (car url) (cdr url))
                           bucket-buttons))))
          
          (letrec ((b (button 0 0 options-frame mode #'(lambda () (setf (ltk:text b) (setf mode (next-mode mode))))))
                   (ch (checkbox 1 0 options-frame "Show word counts?" #'(lambda (a)
                                                                           (declare (ignore a))
                                                                           (setf show-word-counts? (ltk:value ch))
                                                                           (redraw-subclasses))))
                   (ch2 (checkbox 2 0 options-frame "Show file counts?" #'(lambda (a)
                                                                            (declare (ignore a))
                                                                            (setf show-file-counts? (ltk:value ch2))
                                                                            (redraw-subclasses))))
                   (b2 (button 0 1 options-frame "Rebuild corpus" #'rebuild-corpus))
                   (b3 (button 1 1 options-frame "Rebuild text" #'build-text-database))
                   (b4 (button 2 1 options-frame "Rebuild core text" #'build-core-text-database))
                   (ch3 (checkbox 0 0 detail-frame "Word details?" #'(lambda (a)
                                                                       (declare (ignore a))
                                                                       (setf show-word-details? (ltk:value ch3))
                                                                       (redraw-subclasses))))
                   (placeholder-widget nil)
                   (ch4 (checkbox 1 0 detail-frame "Show files?" #'(lambda (a)
                                                                     (declare (ignore a))
                                                                     (setf show-files? (ltk:value ch4))
                                                                     (if (and placeholder-widget show-files?)
                                                                         (ltk:destroy placeholder-widget)
                                                                         (setf placeholder-widget (empty-widget 0 0 file-frame)))
                                                                     (if show-files?
                                                                         (setf files-label (label 0 0 file-frame "FILES"))
                                                                         (ltk:destroy files-label))
                                                                     (redraw-files))))
                   (e3 (entry 6 0 options-frame)))
            (button 7 0 options-frame "Create class" #'(lambda ()
                                                         (let ((txt (ltk:text e3)))
                                                           (if (equal txt "")
                                                               (warning-box "Empty class names are a BAD IDEA." "NOPE")
                                                               (let ((new-class (concat current-class txt "/")))
                                                                 (ensure-directories-exist (full-path new-class))
                                                                 (overwrite-class-file new-class "urls" nil)
                                                                 (overwrite-class-file new-class "comment" "")
                                                                 (rebuild-corpus new-class)
                                                                 (setf (ltk:text e3) "")
                                                                 (change-current-class (concat current-class txt "/")))))))
            (setf (ltk:value ch) show-word-counts?)
            (setf (ltk:value ch2) show-file-counts?)
            (setf (ltk:value ch3) show-word-details?)
            (setf (ltk:value ch4) show-files?))
          (label 0 0 bucket-frame "BUCKET")
          (button 2 0 bucket-frame "Add to bucket" #'(lambda ()
                                                       (add-to-bucket (ltk:text e) nil)
                                                       (setf (ltk:text e) "")))
          (label 0 0 class-frame "CLASSES")    
          (button 1 0 comment-frame "Save comment" #'(lambda ()
                                                       (setf old-comment (read-text-widget tex))
                                                       (overwrite-class-file current-class "comment" (read-text-widget tex))))
          (setf parent-button (button 1 0 class-frame ".." #'(lambda () (change-current-class (parent-class current-class)))))
          (change-class-confirmed current-class)
          (redraw-files)
          fr)))))
