#|
TBD: Autoclose explainers when a thing is classed.
TBD: Remove a file from the downloaded stuffs when removed

Some sites use scripts to deliver boilerplate. While this is not a problem for classification (just ignore them or input manually), it might mess up the crawler.
TBD: There are tons of built in useful sequence functions (positions, search, find...). Check if they could simplify!

Naming convention: 'class' is simplified path, 'folder' is actual folder.
|#

;;; NORMAL STUFFS
;;;----------------------------------------------------------------------------------------------
;;; GUI

(defparameter *modes-cycle* (list 'move 'remove 'nothing))

(defun next-mode (mode)
  (fallback (second (member mode *modes-cycle* :test #'equal))
            (first *modes-cycle*)))

(defun destroy-widgets (list)
  (dolist (w list)
    (ltk:destroy w)))

(defun comment (r c master)
  (text r c master (read-comment "/") 15 35 "NotoMono 10"))

(defun parent-button-state (class)
  (if (equal class
             "/")
      :disabled
      :normal))

(defun existing-class? (class)
  (directory (full-path class)))

(defun recursive-create-class (class)
  (if (not (existing-class? class))
      (let ((parent (parent-class class)))
        (recursive-create-class parent)
        (create-class class))))

(defun create-class (class)
  (ensure-directories-exist (full-path class))
  (overwrite-class-file class "urls" nil)
  (overwrite-class-file class "comment" "")
  (rebuild-corpus class))

(defun action (url origin class mode)
  (case mode
    (move
     ;; needlessly checks for continued existence of file
     (remove-url url origin)
     (redownload-file url)
     (add-url url class))
    (remove
     (remove-url url origin))
    (nothing
     nil)))

(defmacro warn-on-error ((error-title) &body body)
  (let ((title-var (gensym)))
    `(let ((,title-var ,error-title))
       (handler-case (progn
                       ,@body)
         (error (err-text)
           (warning-box err-text ,title-var)
           (abort))))))

(let ((current-class "/")
      (refresh-bucket #'pass)
      (refresh-files #'pass)
      (refresh-classes #'pass)
      (refresh-comment #'pass)
      (save-description #'pass)
      (bucket-urls nil)
      (W nil))
  ;; shared environment variables for parts of GUI

  ;; utils
  (defun add-here (entry)
    (warn-on-error ("Website error")
      (let ((words (word-count (tokens (read-text (ltk:text entry))))))
        (if (< words *min-word-count*)
            (warning-box (concat "This file is very short! Word count: " words ". File will be added, remove it if you consider this an error.") "Few words!")))
      (add-url (ltk:text entry) current-class)
      (setf (ltk:text entry) "")
      (funcall refresh-files)))

  (defun change-current-class (new-class)
    (funcall save-description)
    (setf current-class new-class)
    (ltk:wm-title W new-class)
    (funcall refresh-classes)
    (funcall refresh-files)
    (funcall refresh-comment))
    

  (defun comment-frame (r c master)
    (let* ((fr (frame r c master))
           (comment (comment 0 0 fr))
           (weight-entry (entry 1 0 fr)))
      (button 1 1 fr "Reload saved class description" #'(lambda () (funcall refresh-comment)))
      (labels ((refresh ()
                 (setf (ltk:text comment) (read-comment current-class))
                 (setf (ltk:text weight-entry) (get-weight current-class)))

               (save-description ()
                 (overwrite-class-file current-class "comment" (read-text-widget comment))
                 (warn-on-error ("Invalid weight, not saved")
                   (let ((new-weight (read-from-string (ltk:text weight-entry))))
                     (assert (typep new-weight 'number))
                     (set-weight current-class new-weight)))))
        
        (values #'refresh #'save-description))))

  (defun bucket-frame (r c master)
    (let* ((fr (frame r c master))
           bucket-buttons
           (mode (first *modes-cycle*)))
      
      (letrec ((b (button 0 1 fr mode #'(lambda () (setf (ltk:text b) (setf mode (next-mode mode)))))))
        (labels ((refresh ()
                  (destroy-widgets bucket-buttons)
                  (setf bucket-buttons nil)
                  (dolist (url bucket-urls)
                    (push (new-bucket-button (car url) (cdr url))
                          bucket-buttons)))
               
                (new-bucket-button (url origin)
                  (let ((index (length bucket-buttons)))
                    (button index
                            0
                            fr
                            (cons url origin)
                            #'(lambda ()
                                (action url origin current-class mode)
                                (setf bucket-urls (remove-nth index bucket-urls))
                                (funcall refresh-files)
                                (refresh))))))
         #'refresh))))
  
  (defun file-frame (r c master)
    (let* ((fr (frame r c master))
           (file-list (frame 0 0 fr))
           (e (entry 1 0 fr)))
      (button 2 0 fr "Add to this class" #'(lambda ()
                                             (add-here e)))
      (labels ((add-to-bucket (url origin)
                 (if (find url bucket-urls :key #'car :test #'equal)
                     (warning-box "File already in bucket." "Nope!")
                     (setf bucket-urls (append1 bucket-urls (cons url origin))))
                 (funcall refresh-bucket))
               
               (refresh ()
                 (let ((urls (class-urls current-class)))
                   (ltk:destroy file-list)
                   (setf file-list
                         (scrollable-list 0 0 fr *entries-per-page* urls
                                          (mapcar #'(lambda (url)
                                                      (lambda () (add-to-bucket url current-class)))
                                                  urls))))))
        
        #'refresh)))

  (defun new-class-frame (r c master)
    (let* ((fr (frame r c master))
           (e (entry 0 0 fr)))
      (button 0 1 fr "Create class"
              #'(lambda ()
                  (let* ((name (ltk:text e)))
                    (if (equal name "")
                        (warning-box "Class name cannot be empty." "NOPE")
                        (let ((new-class (concat current-class name "/")))
                          (create-class new-class)
                          (setf (ltk:text e) "")
                          (change-current-class new-class))))))
      fr))

  (defun class-data (starting-show? c settings-c master title checkbox-title value-fun)
    (let (label-list
          (show? starting-show?))
      (labels ((refresh ()
                 (destroy-widgets label-list)
                 (setf label-list
                       (if show?
                           (let ((acc (list (label 0 c master title)))
                                 (counter 0))
                             (dolist (subclass (subclasses current-class))
                               (push (label (incf counter) c master (funcall value-fun subclass))
                                     acc))
                             acc)))))
        (letrec ((ch (checkbox c settings-c master checkbox-title #'(lambda (a)
                                                             (declare (ignore a))
                                                             (setf show? (ltk:value ch))
                                                             (refresh)))))
          (setf (ltk:value ch) show?)
          
          #'refresh))))

  (defun class-data-frame (c settings-c master)
    ;; Not actually a frame. Merely fills in a data type. Name is mostly driven from convention and outside appearance.
    (let* ((e (entry (+ c 3) settings-c master))

           (refresh-words (class-data t c settings-c master "WORDS" "Show word counts?" #'get-word-count))
           (refresh-files (class-data t (+ c 1) settings-c master "FILES" "Show file counts?" #'get-file-count))
           (refresh-usage (class-data nil (+ c 2) settings-c master "ALGUMA COISA" "Show word usage?" #'(lambda (class) (let ((word (intern (ltk:text e))))
                                                                                                           (concat (my-round (occurrences word (get-recursive-corpus class)))
                                                                                                                   " ("
                                                                                                                   (my-round (* 10000 (/ (occurrences word (get-recursive-corpus class))
                                                                                                                                         (get-word-count class))))
                                                                                                                   " ‱)"))))))
      #'(lambda ()
          (funcall refresh-words)
          (funcall refresh-files)
          (funcall refresh-usage))))

  (defun class-frame (r c master)
    (let* ((fr (frame r c master))
           subclass-buttons
           parent-button ;; recursively dependent with refresh, so we have to set it later
           (class-frame (frame 0 0 fr))
           (refresh-class-data (class-data-frame 1 4 class-frame)))
      
      (new-class-frame 1 0 fr)
      
      (labels ((refresh ()
                 (ltk:configure parent-button :state (parent-button-state current-class))
                 (destroy-widgets subclass-buttons)
                 (setf subclass-buttons nil)
                 (let ((subclasses (subclasses current-class))
                       (counter 0))
                   (dolist (subclass subclasses)
                     (incf counter)
                     (push (button counter 0
                                   class-frame
                                   (folder-name subclass)
                                   #'(lambda ()
                                       (change-current-class subclass)))
                           subclass-buttons)))
                 (funcall refresh-class-data)))

        (setf parent-button (button 0 0 class-frame ".." #'(lambda () (change-current-class (parent-class current-class)))))
        
        #'refresh)))

  (defun rebuild-frame (r c master)
    (let ((fr (frame r c master)))
      (button 0 0 fr "Rebuild corpus" #'(lambda ()
                                          (let ((timer (get-internal-real-time)))
                                            (build-core-text-database)
                                            (rebuild-corpus)
                                            (show-time timer "Rebuilt the corpus.")
                                            (funcall refresh-classes))))
      (button 1 0 fr "Rebuild text" #'build-text-database)
      (button 2 0 fr "Rebuild core text" #'(lambda ()
                                             (let ((timer (get-internal-real-time)))
                                               (build-core-text-database)
                                               (show-time timer "Rebuilt the core text database."))))
      fr))

  (defun db-window ()
    (setf W (window "/"))
    (let ((class-frame (frame 0 0 W)))
      (ltk:on-close W #'(lambda () (ltk:destroy ltk:*tk*)))

      (multiple-value-bind (comment-refresher description-saver)
          (comment-frame 0 1 class-frame)
        (setf refresh-comment comment-refresher)
        (setf save-description description-saver)
        (let* ((file-refresher (file-frame 1 0 W))
               (bucket-refresher (bucket-frame 1 1 W))
               (class-refresher (class-frame 0 0 class-frame)))

          (button 3 0 W "Go to classifier" #'(lambda ()
                                               (ltk:destroy W)
                                               (classifier-window)))
          (rebuild-frame 2 0 W)

          (setf refresh-files file-refresher)
          (setf refresh-bucket bucket-refresher)
          (setf refresh-classes class-refresher)
          
          (funcall refresh-comment)
          (funcall refresh-classes)
          (funcall refresh-bucket)
          (funcall refresh-files)
          W)))))

(defun db ()
  (ltk:with-ltk ()
    (ltk:withdraw ltk:*tk*)
    (db-window)))
