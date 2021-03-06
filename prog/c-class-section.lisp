(let ((add-to-bucket #'pass)
      (refresh-classes #'pass)
      (refresh-config #'pass)
      (rename #'pass)
      (save-description #'pass)
      (refresh-files #'pass))

  (labels ((set-refresher (fun)
             (setf refresh-files fun))

           (change-current-class (new-class)
             (funcall save-description)
             
             (let ((old-current-class (get-current-class)))
               (funcall rename)
               (set-current-class (new-path old-current-class (get-current-class) new-class)))
             (funcall refresh-classes)
             (funcall refresh-files)
             (funcall refresh-config))

           (comment-frame (r c master)
             (let* ((fr (frame r c master))
                    (comment (comment 0 0 fr))
                    (fr2 (frame 1 0 fr))
                    (weight-entry (described-entry 0 0 fr2 "Weight: ")))
               (button 2 0 fr "Reload saved class description" #'(lambda () (funcall refresh-config)))
               (letrec ((tentative-ch (checkbox 0 1 fr2 "Tentative?" #'(lambda ()
                                                                         (set-tentative (get-current-class) (ltk:value tentative-ch))))))
                 (labels ((refresh ()
                            (setf (ltk:text comment) (read-comment (get-current-class)))
                            (setf (ltk:value tentative-ch) (read-tentative (get-current-class)))
                            (setf (ltk:text weight-entry) (read-weight (get-current-class))))

                          (save-description ()
                            (set-comment (get-current-class) (read-text-widget comment))
                            (warn-on-error ("Invalid weight, not saved")
                              (let ((new-weight (read-from-string (ltk:text weight-entry))))
                                (assert (typep new-weight 'number))
                                (set-weight (get-current-class) new-weight)))))
                 
                   (values #'refresh #'save-description)))))

           (new-class-frame (r c master)
             (let* ((fr (frame r c master))
                    (e (entry 0 0 fr)))
               (button 0 1 fr "Create class"
                       #'(lambda ()
                           (let* ((name (ltk:text e)))
                             (if (equal name "")
                                 (warning-box "Class name cannot be empty." "NOPE")
                                 (let ((new-class (concat (get-current-class) name "/")))
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
                                      (dolist (subclass (subclasses (get-current-class)))
                                        (push (label (incf counter) c master (funcall value-fun subclass))
                                              acc))
                                      acc)))))
                 (letrec ((ch (checkbox c settings-c master checkbox-title #'(lambda ()
                                                                               (setf show? (ltk:value ch))
                                                                               (refresh)))))
                   (setf (ltk:value ch) show?)
                   
                   #'refresh))))

           (class-data-frame (c settings-c master reduced?)
             ;; Not actually a frame. Merely fills in a data type. Name is mostly driven by convention and outside appearance.
             (let ((refresh-files (class-data t (+ c (if reduced? 0 1)) settings-c master "FILES" "Show file counts?" #'get-file-count)))
               (if reduced?
                   refresh-files
                   (let* ((e (entry (+ c 3) settings-c master))
                          (refresh-words (class-data t c settings-c master "WORDS" "Show word counts?" #'get-word-count))
                          (refresh-usage (class-data nil (+ c 2) settings-c master "WORD USAGE" "Show word usage?" #'(lambda (class) (let ((word (intern (ltk:text e))))
                                                                                                                                       (concat (my-round (occurrences word (get-recursive-corpus class)))
                                                                                                                                               " ("
                                                                                                                                               (my-round (* 10000 (/ (occurrences word (get-recursive-corpus class))
                                                                                                                                                                     (get-word-count class))))
                                                                                                                                               " ‱)"))))))
                     
                     #'(lambda ()
                         (funcall refresh-files)
                         (funcall refresh-words)
                         (funcall refresh-usage))))))

           (class-frame (r c master reduced?)
             (let* ((fr (frame r c master))
                    subclass-buttons
                    parent-button ;; recursively dependent with refresh, so we have to set it later
                    (class-frame (frame 1 0 fr))
                    (refresh-class-data (class-data-frame 1 4 class-frame reduced?))
                    (current-class-frame (frame 0 0 fr))
                    (class-loc (label 0 0 current-class-frame "/"))
                    (class-name (entry 0 1 current-class-frame (folder-name (get-current-class))))
                    (modes-cycle (list 'enter 'edit))
                    (mode (first modes-cycle)))
               
               (new-class-frame 2 0 fr)
               
               (letrec ((mode-frame (frame 100 0 fr))
                        (b (button 0 1 mode-frame mode #'(lambda () (setf (ltk:text b) (setf mode (next-mode mode modes-cycle)))))))
                 (label 0 0 mode-frame "On clicking a class button: ")
                 (labels ((refresh ()
                            (ltk:configure parent-button :state (parent-button-state (get-current-class)))
                            (destroy-widgets subclass-buttons)
                            (setf subclass-buttons nil)
                            (setf (ltk:text class-name) (folder-name (get-current-class)))
                            (setf (ltk:text class-loc) (concat "Current class: " (fallback (parent-class (get-current-class)) "/")))
                            
                            (let ((subclasses (subclasses (get-current-class)))
                                  (counter 0))
                              (dolist (subclass subclasses)
                                (incf counter)
                                (push (button counter 0
                                              class-frame
                                              (folder-name subclass)
                                              #'(lambda ()
                                                  (if (equal mode 'enter)
                                                      (change-current-class subclass)
                                                      (funcall add-to-bucket nil subclass))))
                                      subclass-buttons)))
                            (funcall refresh-class-data))
                          (rename ()
                            (warn-on-error ("Bad class name")
                              (let ((new-name (ltk:text class-name)))
                                (if (not (or (equal (get-current-class) "/")
                                             (equal new-name (folder-name (get-current-class)))))
                                    (progn
                                      (rename-class (get-current-class) new-name)
                                      (set-current-class (path-after-renaming (get-current-class) new-name))))))))

                   (setf parent-button (button 0 0 class-frame ".." #'(lambda () (change-current-class (parent-class (get-current-class))))))
                   
                   (values #'refresh #'rename))))))

    (defun class-section (r c master &optional reduced?)
      (let ((fr (frame r c master)))
        (multiple-value-bind (refresher updater)
            (comment-frame 0 2 fr)
          (setf refresh-config refresher)
          (setf save-description updater))
        (multiple-value-bind (refresher renamer)
            (class-frame 0 0 fr reduced?)
          (setf refresh-classes refresher)
          (setf rename renamer))
        (setf add-to-bucket (bucket-section 0 1 fr
                                            #'(lambda (url origin current-class mode)
                                                (declare (ignore url))
                                                (case mode
                                                  (move
                                                   (print (concat "Moving " origin " to " (concat current-class (folder-name origin) "/")))
                                                   (if (equal 0 (search origin current-class))
                                                       (warning-box "You are trying to move a class into itself." "Nope!")
                                                       (progn
                                                         (move-class origin (concat current-class (folder-name origin) "/")))))
                                                  (remove
                                                   (remove-class origin))
                                                  (nothing nil))
                                                (funcall refresh-classes))
                                            (list 'move 'remove 'nothing)
                                            #'(lambda (url origin)
                                                (declare (ignore url))
                                                origin)))
        
        (values #'(lambda ()
                    (funcall refresh-classes)
                    (funcall refresh-config))
                #'set-refresher)))))
