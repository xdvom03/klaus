(defun overwrite-direct-file (path &rest things)
  (ensure-directories-exist path)
  (with-open-file (stream path :direction :output :if-exists :supersede :if-does-not-exist :create)
    (dolist (thing things)
      (print thing stream))))

(defun overwrite-file (class file-name &rest things)
  (apply #'overwrite-direct-file (concat (full-path class) file-name) things))

(defun read-from-file (path)
  (with-open-file (stream path :direction :input :if-does-not-exist :error)
    (read stream)))

(defun file-content (file-name)
  (read-from-file (concat *files-folder* (file-alias file-name))))

(defun link-occurrences (link &optional (class "/"))
  ;; returns all folders where the link is present
  (remove-if #'null
             (append1 (apply #'append (mapcar #'(lambda (subclass) (link-occurrences link subclass))
                                              (subclasses class)))
                      (if (member link (recursive-links class) :test #'equal)
                          class))))

(defun read-comment (class)
  ;; Side effect: Creates the comment file if there is none
  (if (directory (concat (full-path class) "comment"))
      (read-from-file (concat (full-path class) "comment"))
      (overwrite-file class "comment" "ňyc")))

(defun class-links (class)
  (read-from-file (concat (full-path class) "links")))

(defun recursive-links (class)
  (append (class-links class)
          (apply #'append
                 (mapcar #'recursive-links
                         (subclasses class)))))

;;; READING FILES
;;;----------------------------------------------------------------------------------------------
;;; CONVERTORS

(defun simplified-path (path)
  (let* ((path-parts (split path (char "/" 0)))
         (varying-part (cdr (member "classes" path-parts :test #'equal))))
    (reduce #'(lambda (a b) (concat a "/" b))
            ;; Prepend an empty string in case of the home folder - reducing NIL it has no idea what the base state is.
            (cons "" varying-part))))

(defun full-path (simple-path)
  (concat *classes-folder* (subseq simple-path 1)))

;;; CONVERTORS
;;;----------------------------------------------------------------------------------------------
;;; FOLDER NAVIGATION

(defun subclasses (class)
  (mapcar #'simplified-path (remove-if-not #'folder? (mapcar #'namestring (directory (concat (full-path class) "*"))))))

(defun parent-class (class)
  (concat (join (remove-last (remove-last (split class #\/))) "/") "/"))

(defun folder-name (path)
  (second (reverse (split path #\/))))

;;; FOLDER NAVIGATION
;;;----------------------------------------------------------------------------------------------
;;; DOWNLOADING FILES

(defun redownload (class)
  (let ((files (class-links class)))
    (dolist (file files)
      (redownload-file file))
    (dolist (subclass (subclasses class))
      (redownload subclass)))
  (print (concat "redownloaded " class)))

(defun redownload-file (file-name)
  (if (not (file-alias file-name))
      (add-alias file-name (1+ (apply #'max (append1 (used-aliases) 0))))
      ;; TBD: maybe check for newer version?
      )
  (file-alias file-name))

(defun add-alias (name alias)
  (let ((aliases (aliases)))
    (overwrite-direct-file *aliases-file* (append1 aliases (cons name alias)))
    (overwrite-direct-file (concat *files-folder* alias) (safe-fetch-html name))))

(defun file-alias (file-name)
  (gethash file-name (assoc-to-hashtable (aliases))))

(defun aliases ()
  (read-from-file *aliases-file*))

(defun used-aliases ()
  (mapcar #'cdr (aliases)))
