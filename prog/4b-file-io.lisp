;; all file input/output (and data storage)

;; TBD: The reading here (& below) is only needed when recompiling. Remove from final executable.
(let ((comment-tree (ht)))
  (defun read-comments ()
    (setf comment-tree
          (assoc-to-hashtable (read-from-file *comments-file*))))

  (defun save-comments ()
    (overwrite-file *comments-file*
                    (hashtable-to-assoc comment-tree)))

  (defun set-comment (class comment)
    (setf (gethash class comment-tree)
          comment)
    (save-comments))
  
  (defun read-comment (class)
    (gethash class comment-tree))

  (read-comments))

(let ((weight-tree (ht)))
  (defun read-weights ()
    (setf weight-tree (assoc-to-hashtable (read-from-file *weights-file*))))

  (defun save-weights ()
    (overwrite-file *weights-file* (hashtable-to-assoc weight-tree)))

  (defun set-weight (class weight)
    (setf (gethash class weight-tree)
          weight)
    (save-weights))
  
  (defun read-weight (class)
    (gethash class weight-tree))

  (read-weights))

(defun apply-to-all-classes (fun)
  ;; TBD: Add to a better crawler testing suite
  (labels ((res (class)
             (funcall fun class)
             (dolist (subclass (subclasses class))
               (res subclass))))
    (res "/")))

(defun classes-hashtable (fun)
  (let ((acc (ht)))
    (apply-to-all-classes #'(lambda (class)
                              (setf (gethash class acc)
                                    (funcall fun class))))
    acc))












(let ((url-tree (ht))
      (corpus-tree (ht))
      (recursive-corpus-tree (ht))
      (word-count-tree (ht))
      (url-count-tree (ht))
      (subclasses (ht)))

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
  
  (defun add-url (url class)
    (redownload-url url)
    (if (member url (class-urls "/" t) :test #'equal)
        (error (concat "File already in " (location url)))
        (setf (gethash class url-tree)
              (append1 (gethash class url-tree) url)))
    (save-corpora) ;; TBD: autosave for now, unsaved changes warn later if there is any problem with this system
    )
  
  (defun remove-url (url class)
    (setf (gethash class url-tree)
          (remove-if #'(lambda (checked-url)
                         (equal checked-url url))
                     (gethash class url-tree)))
    (save-corpora))

  (defun get-recursive-url-tree (class)
    (reduce #'append
            (append1 (mapcar #'get-recursive-url-tree
                             (subclasses class))
                     (gethash class url-tree))))

  (defun get-recursive-corpus (class)
    (gethash class recursive-corpus-tree))

  (defun get-file-count (class)
    (gethash class url-count-tree))

  (defun get-word-count (class)
    (gethash class word-count-tree))  

  (defun rebuild-corpus (&optional (class "/"))
    (let* ((base-corpus (get-corpus class))
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
            (length (append (recursive-imported-urls class)
                            (get-recursive-url-tree class))))
      (setf (gethash class word-count-tree)
            (word-count recursive-corpus))
      (print (concat "rebuilt: " class))
      (if (equal class "/")
          (save-corpora))
      
      (scale-corpus recursive-corpus (read-weight class))))

  (defun save-corpora ()
    ;; Must be saved so that it matches
    (overwrite-file *urls-file*
                    (hashtable-to-assoc url-tree))
    (overwrite-file *corpora-file*
                    (hashtable-to-assoc (map-to-hash (compose #'sort-corpus
                                                              #'hashtable-to-assoc
                                                              #'(lambda (class)
                                                                  (gethash class corpus-tree)))
                                                     (list-keys corpus-tree)))))

  (defun read-saved ()
    (let ((assoc-corpus (assoc-to-hashtable (read-from-file *corpora-file*))))
      (setf corpus-tree
            (map-to-hash (compose #'assoc-to-hashtable
                                  #'(lambda (class)
                                      (gethash class assoc-corpus)))
                         (list-keys assoc-corpus))))
    (setf url-tree
          (assoc-to-hashtable (read-from-file *urls-file*))))

  (defun build-subclasses ()
    (let ((classes (classes)))
      (dolist (class classes)
        (setf (gethash class subclasses)
              (remove-if-not #'(lambda (class2)
                                 (equal (parent-class class2)
                                        class))
                             classes))))))
