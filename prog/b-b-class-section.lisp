(let ((current-class "/")
      (add-to-bucket #'pass)
      (refresh-classes #'pass)
      (refresh-comment #'pass)
      (rename #'pass)
      (save-description #'pass)
      (refresh-files #'pass)
      (modes-cycle (list 'move 'bucket))
      (bucket-classes nil))

  (labels ((set-refresher (fun)
             (setf refresh-files fun))

           (change-current-class (new-class)
             (funcall save-description)
             (funcall rename)
             (setf current-class new-class)
             (funcall refresh-classes new-class)
             (funcall refresh-files new-class)
             (funcall refresh-comment))

           (next-mode (mode)
             (fallback (second (member mode modes-cycle :test #'equal))
                       (first modes-cycle)))

           (comment-frame (r c master)
             (let* ((fr (frame r c master))
                    (comment (comment 0 0 fr))
                    (weight-entry (entry 1 0 fr)))
               (button 2 0 fr "Reload saved class description" #'(lambda () (funcall refresh-comment)))
               (labels ((refresh ()
                          (setf (ltk:text comment) (read-comment current-class))
                          (setf (ltk:text weight-entry) (read-weight current-class)))

                        (save-description ()
                          (set-comment current-class (read-text-widget comment))
                          (warn-on-error ("Invalid weight, not saved")
                            (let ((new-weight (read-from-string (ltk:text weight-entry))))
                              (assert (typep new-weight 'number))
                              (set-weight current-class new-weight)))))
                 
                 (values #'refresh #'save-description))))

           (new-class-frame (r c master)
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
           (class-data (starting-show? c settings-c master title checkbox-title value-fun)
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

           (class-data-frame (c settings-c master)
             ;; Not actually a frame. Merely fills in a data type. Name is mostly driven by convention and outside appearance.
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

           (class-bucket-frame (r c master)
             (let* ((fr (frame r c master))
                    bucket-buttons)
               
               (labels ((new-bucket-button (class)
                          (if (find class bucket-classes :test #'equal)
                              (warning-box "File already in bucket." "Nope!")
                              (letrec ((index (length bucket-buttons))
                                       (new-button (button index
                                                           0
                                                           fr
                                                           class
                                                           #'(lambda ()
                                                               (move-class class (concat current-class (folder-name class) "/"))
                                                               (setf bucket-classes (remove class bucket-classes :test #'equal))
                                                               (ltk:destroy new-button)
                                                               (funcall refresh-classes current-class)))))
                                (push class bucket-classes)
                                (push new-button bucket-buttons)))))
                 #'new-bucket-button)))

           (class-frame (r c master)
             (let* ((fr (frame r c master))
                    subclass-buttons
                    parent-button ;; recursively dependent with refresh, so we have to set it later
                    (class-frame (frame 1 0 fr))
                    (refresh-class-data (class-data-frame 1 4 class-frame))
                    (class-name (entry 0 0 fr (folder-name current-class)))
                    (mode (first modes-cycle)))
               
               (new-class-frame 2 0 fr)
               
               (letrec ((b (button 0 1 fr mode #'(lambda () (setf (ltk:text b) (setf mode (next-mode mode)))))))
                 (labels ((refresh (new-class)
                            (setf current-class new-class)
                            (ltk:configure parent-button :state (parent-button-state current-class))
                            (destroy-widgets subclass-buttons)
                            (setf subclass-buttons nil)
                            (setf (ltk:text class-name) (folder-name current-class))
                            (let ((subclasses (subclasses current-class))
                                  (counter 0))
                              (dolist (subclass subclasses)
                                (incf counter)
                                (push (button counter 0
                                              class-frame
                                              (folder-name subclass)
                                              #'(lambda ()
                                                  (if (equal mode 'move)
                                                      (change-current-class subclass)
                                                      (funcall add-to-bucket subclass))))
                                      subclass-buttons)))
                            (funcall refresh-class-data))
                          (rename ()
                            (warn-on-error ("Bad class name")
                              (let ((new-name (ltk:text class-name)))
                                (if (not (or (equal current-class "/")
                                             (equal new-name (folder-name current-class))))
                                    (progn
                                      (rename-class current-class new-name)
                                      ;; TBD: Duplication here and 4b renaming code
                                      (setf current-class (concat (parent-class current-class) new-name "/"))))))))

                   (setf parent-button (button 0 0 class-frame ".." #'(lambda () (change-current-class (parent-class current-class)))))
                   
                   (values #'refresh #'rename))))))

    (defun class-section (r c master)
      (let ((fr (frame r c master)))
        (setf add-to-bucket (class-bucket-frame 0 2 fr))
        (multiple-value-bind (refresher updater)
            (comment-frame 0 1 fr)
          (setf refresh-comment refresher)
          (setf save-description updater))
        (multiple-value-bind (refresher renamer)
            (class-frame 0 0 fr)
          (setf refresh-classes refresher)
          (setf rename renamer))
        
        (values #'(lambda (&optional (new-class current-class))
                    (funcall refresh-classes new-class)
                    (funcall refresh-comment))
                #'set-refresher)))))
