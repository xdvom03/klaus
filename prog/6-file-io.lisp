;; all file input/output (and data storage)

(defun apply-to-all-classes (fun)
  ;; TBD: Add to a better crawler testing suite
  (labels ((rec (class)
             (funcall fun class)
             (dolist (subclass (subclasses class))
               (rec subclass))))
    (rec "/")))

(defun classes-hashtable (fun)
  (let ((acc (ht)))
    (apply-to-all-classes #'(lambda (class)
                              (setf (gethash class acc)
                                    (funcall fun class))))
    acc))

(defun move-hash (hash-table original-key new-key)
  ;; destructive (which is the point)
  ;; hash tables have limited test options, all of which are commutative, so we need not worry about the order here
  (multiple-value-bind (value exists?)
      (gethash original-key hash-table)
    (if (and exists?
             (not (funcall (hash-table-test hash-table) original-key new-key)))
        (progn
          (setf (gethash new-key hash-table)
                value)
          (remhash original-key hash-table)
          hash-table))))

(defun new-path (old-path new-path subclass)
  ;; When the old path is moved to a new path, what is the new path of its subclass?
  (concat new-path (subseq subclass (length old-path))))

(let ((subclasses (ht))
      (url-tree (ht))
      (corpus-tree (ht))
      (recursive-corpus-tree (ht))
      (word-count-tree (ht))
      (url-count-tree (ht))

      (cached-corpora (ht))
      (cached-urls (ht))
      
      (comment-tree (ht))
      (weight-tree (ht))
      (tentative-tree (ht)))

  (defun read-comments ()
    (setf comment-tree
          (assoc-to-hashtable (read-from-file *comments-file*))))

  (defun save-comments ()
    (overwrite-file *comments-file*
                    (hashtable-to-assoc comment-tree)))

  (defun set-comment (class comment)
    (setf (gethash class comment-tree)
          comment))
  
  (defun read-comment (class)
    (gethash class comment-tree))
  
  
  (defun read-weights ()
    (setf weight-tree (assoc-to-hashtable (read-from-file *weights-file*))))

  (defun save-weights ()
    (overwrite-file *weights-file* (hashtable-to-assoc weight-tree)))

  (defun set-weight (class weight)
    (setf (gethash class weight-tree)
          weight))

  (defun read-weight (class)
    (gethash class weight-tree))

  
  (defun read-tentative (class)
    (gethash class tentative-tree))

  (defun read-tentatives ()
    (setf tentative-tree (assoc-to-hashtable (read-from-file *tentatives-file*))))

  (defun save-tentatives ()
    (overwrite-file *tentatives-file* (hashtable-to-assoc tentative-tree)))

  (defun set-tentative (class tentative?)
    (setf (gethash class tentative-tree)
          tentative?))

  (defun save-config ()
    (save-comments)
    (save-weights)
    (save-tentatives))

  (defun read-config ()
    (read-comments)
    (read-weights)
    (read-tentatives))

  ;;; CONFIG
  ;;;----------------------------------------------------------------------------------------------
  ;;;

  (defun read-cached-urls (class)
    (gethash class cached-urls))

  (defun move-one-class (original-path new-path)
    (move-hash url-tree original-path new-path)
    (move-hash corpus-tree original-path new-path)
    (move-hash recursive-corpus-tree original-path new-path)
    (move-hash word-count-tree original-path new-path)
    (move-hash url-count-tree original-path new-path)
    (move-hash comment-tree original-path new-path)
    (move-hash weight-tree original-path new-path)
    (move-class-imports original-path new-path))

  (defun remove-one-class (path)
    (remhash path url-tree)
    (remhash path corpus-tree)
    (remhash path recursive-corpus-tree)
    (remhash path word-count-tree)
    (remhash path url-count-tree)
    (remhash path comment-tree)
    (remhash path weight-tree))

  (defun recursive-subclasses (class)
    ;; includes the class itself
    (append1 (reduce #'append (mapcar #'recursive-subclasses (gethash class subclasses)))
             class))

  (defun remove-class (path)
    ;; class is the class path, while new-name is the new endpoint name
    (mapcar #'(lambda (subclass)
                (remove-one-class subclass))
            (recursive-subclasses path))
    (build-subclasses))
  
  (defun move-class (old-path new-path)
    ;; class is the class path, while new-name is the new endpoint name
    (dolist (subclass (recursive-subclasses old-path))
      (move-one-class subclass
                      (new-path old-path new-path subclass)))
    (build-subclasses))

  (defun rename-class (class new-name)
    ;; class is the class path, while new-name is the new endpoint name
    ;; TBD: Rename all imports
    (if (find #\/ new-name)
        (error "Name contains slash.")
        (move-class class (concat (parent-class class) new-name "/"))))

  (defun classes ()
    (remove-duplicates (append (list-keys url-tree)
                               (imported-classes))
                       :test #'equal))
  
  (defun class-urls (class &optional recursive?)
    (append (gethash class url-tree)
            (if recursive?
                (apply #'append
                       (mapcar #'(lambda (class) (class-urls class t))
                               (subclasses class))))))

  (defun subclasses (class)
    (gethash class subclasses))
  
  (defun create-class (class)
    (set-weight class 1)
    (set-comment class "")
    (setf (gethash class url-tree)
          nil)
    (let ((parent (parent-class class)))
      (setf (gethash parent subclasses)
            (append1 (gethash parent subclasses)
                     class)))
    ;; sets up corpus, url/word count
    (rebuild-corpus class))

  (defun add-manual-url (url class)
    (if (member url (class-urls "/" t) :test #'equal)
        (error (concat "File already in " (location url)))
        (setf (gethash class url-tree)
              (append1 (gethash class url-tree) url))))
  
  (defun add-url (url class)
    (redownload-url url)
    (if (member url (class-urls "/" t) :test #'equal)
        (error (concat "File already in " (location url)))
        (setf (gethash class url-tree)
              (append1 (gethash class url-tree) url))))
  
  (defun remove-url (url class)
    (setf (gethash class url-tree)
          (remove-if #'(lambda (checked-url)
                         (equal checked-url url))
                     (gethash class url-tree))))

  (defun get-recursive-corpus (class)
    ;; includes imports
    (gethash class recursive-corpus-tree))

  (defun get-file-count (class)
    (gethash class url-count-tree))

  (defun get-word-count (class)
    (gethash class word-count-tree))

  (defun rebuild-corpus (&optional (class "/"))
    (let* ((urls (class-urls class))
           (base-corpus (let ((cached (gethash class cached-corpora)))
                          (cond
                            ((null urls)
                             (ht))
                            ((and cached
                                  (equal urls (gethash class cached-urls)))
                             cached)
                            (t
                             (print (concat "EDIT at " class))
                             (get-corpus class)))))
           (corpus (add-corpuses (imported-corpus class)
                                 base-corpus))
           (recursive-corpus (reduce #'add-corpuses
                                     (append1 (mapcar #'rebuild-corpus
                                                      (subclasses class))
                                              corpus)
                                     :initial-value (ht))))
      (setf (gethash class corpus-tree)
            base-corpus)
      (setf (gethash class recursive-corpus-tree)
            recursive-corpus)
      (setf (gethash class url-count-tree)
            (+ (length (recursive-imported-urls class))
               (length (class-urls class t))))
      (setf (gethash class word-count-tree)
            (word-count recursive-corpus))
      
      (scale-corpus recursive-corpus (read-weight class))))

  (defun rebuild-url-counts ()
    (apply-to-all-classes #'(lambda (class)
                              (setf (gethash class url-count-tree)
                                    (+ (length (recursive-imported-urls class))
                                       (length (class-urls class t)))))))

  (defun save-edited-discovered ()
    (overwrite-file *discovered-file*
                    (hashtable-to-assoc url-tree)))

  (defun save-corpora ()
    ;; Must be saved so that it matches
    (overwrite-file *urls-file*
                    (hashtable-to-assoc url-tree))
    (overwrite-file *corpora-file*
                    (saveable-corpora corpus-tree))
    (setf cached-corpora (alexandria:copy-hash-table corpus-tree))
    (setf cached-urls (alexandria:copy-hash-table url-tree)))

  (defun saved-urls ()
    (assoc-to-hashtable (read-from-file *urls-file*)))

  (defun discovered-urls ()
    (assoc-to-hashtable (read-from-file *discovered-file*)))

  (defun saved-corpora ()
    (let ((assoc-corpus (assoc-to-hashtable (read-from-file *corpora-file*))))
      (map-to-hash (compose #'assoc-to-hashtable
                            #'(lambda (class)
                                (gethash class assoc-corpus)))
                   (list-keys assoc-corpus))))

  (defun read-saved ()
    (let ((corpora (saved-corpora))
          (urls (saved-urls)))
      (setf corpus-tree corpora)
      (setf cached-corpora (alexandria:copy-hash-table corpora))
      (setf url-tree urls)
      (setf cached-urls (alexandria:copy-hash-table urls))))

  (defun read-discovered ()
    (let ((urls (discovered-urls)))
      (setf url-tree urls)
      (setf cached-urls (alexandria:copy-hash-table urls))))

  (defun build-subclasses ()
    (let ((classes (classes)))
      (dolist (class classes)
        (setf (gethash class subclasses)
              (sort (copy-seq (remove-if-not #'(lambda (class2)
                                                 (equal (parent-class class2)
                                                        class))
                                             classes))
                    #'string<))))))

;;; I/O HANDLING
;;;----------------------------------------------------------------------------------------------
;;; FILE POSITION SEARCH

(defun location (url &optional (class "/"))
  ;; returns the class where the url is to be found
  (if (member url (class-urls class) :test #'equal)
      class
      (let ((sub (find-if #'(lambda (subclass) (member url (class-urls subclass t) :test #'equal))
                          (subclasses class))))
        (if sub
            (location url sub)))))

;;; VARIOUS UTILS
;;;----------------------------------------------------------------------------------------------
;;; GENERAL BUCKET SYSTEM

(defun next-mode (mode modes-cycle)
  (fallback (second (member mode modes-cycle :test #'equal))
            (first modes-cycle)))

(defun bucket-section (r c master button-action modes-cycle data-fun)
  ;; data-fun is called as (data-fun url origin) to produce button labels
  (let* ((f (frame r c master))
         (fr (frame 1 0 f))
         bucket-buttons
         bucket-data
         (mode (first modes-cycle)))
    (label 0 0 f "On clicking an entry: ")
    (letrec ((b (button 0 1 f mode #'(lambda () (setf (ltk:text b) (setf mode (next-mode mode modes-cycle)))))))
      (labels ((new-bucket-button (url origin)
                 (let ((button-text (funcall data-fun url origin)))
                   (if (find button-text bucket-data :test #'equal)
                       (warning-box "Already in bucket." "Nope!")
                       (letrec ((index (length bucket-buttons))
                                (new-button (button index
                                                    0
                                                    fr
                                                    button-text
                                                    #'(lambda ()
                                                        (funcall button-action url origin (get-current-class) mode)
                                                        (setf bucket-data (remove button-text bucket-data :test #'equal))
                                                        (ltk:destroy new-button)))))
                         (push button-text bucket-data)
                         (push new-button bucket-buttons))))))
        #'new-bucket-button))))
