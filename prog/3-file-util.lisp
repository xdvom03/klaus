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
  (let* ((path-parts (cl-strings:split path #\/))
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
  (concat (cl-strings:join (remove-last (remove-last (cl-strings:split class #\/))) :separator "/") "/"))

(defun folder-name (path)
  (second (reverse (cl-strings:split path))))

;;; FOLDER NAVIGATION
;;;----------------------------------------------------------------------------------------------
;;; DOWNLOADING FILES

(defun redownload (class)
  (ensure-directories-exist *files-folder*)
  (if (not (directory *aliases-file*))
      (overwrite-direct-file *aliases-file* nil))
  (let ((files (class-links class)))
    (dolist (file files)
      (redownload-file file))
    (dolist (subclass (subclasses class))
      (redownload subclass)))
  (print (concat "redownloaded " class)))

(defun redownload-file (file-name)
  (if (not (file-alias file-name))
      (add-alias file-name)
      ;; TBD: maybe check for newer version?
      )
  (file-alias file-name))

;; TBD: Generalise to alias utils
(defun add-alias (name)
  (let* ((aliases (aliases))
         (new-alias (1+ (apply #'max (append1 (mapcar #'cdr aliases) 0))))
         (html (safe-fetch-html name)))
    (overwrite-direct-file *aliases-file* (append1 aliases (cons name new-alias)))
    (overwrite-direct-file (concat *html-folder* new-alias) html)
    (overwrite-direct-file (concat *text-folder* new-alias) (extract-text html))))

(defun file-alias (file-name)
  (gethash file-name (assoc-to-hashtable (aliases))))

(defun aliases ()
  (read-from-file *aliases-file*))

;; TBD: Useless?
(defun used-aliases ()
  (mapcar #'cdr (aliases)))

(defun read-core-text (url)
  (read-from-file (concat *core-text-folder* (file-alias url))))

(defun read-text (url)
  (read-from-file (concat *text-folder* (file-alias url))))

(defun read-html (url)
  (read-from-file (concat *html-folder* (file-alias url))))

(defun domain-aliases ()
  (read-from-file *domain-aliases-file*))

(defun domain-alias (domain)
  (gethash domain (assoc-to-hashtable (domain-aliases))))

(defun read-domain-links (domain)
  (let ((path (concat *domain-lists-folder* (domain-alias domain))))
    (if (directory path)
        (read-from-file path))))

(defun read-domain-boilerplate (domain)
  (let ((path (concat *boilerplate-folder* (domain-alias domain))))
    (if (directory path)
        (read-from-file path))))

(defun add-domain-alias (domain)
  (let* ((aliases (domain-aliases))
         (new-alias (1+ (apply #'max (append1 (mapcar #'cdr aliases) 0)))))
    (overwrite-direct-file *domain-aliases-file* (append1 aliases (cons domain new-alias)))))

(defun readd-domain-alias (domain)
  (if (not (domain-alias domain))
      (add-domain-alias domain))
  (domain-alias domain))

;;; DOWNLOADING FILES
;;;----------------------------------------------------------------------------------------------
;;;

(defun apply-to-all-classes (fun)
  ;; not used in code yet, but useful to reset weights or stuff
  (labels ((res (class)
             (funcall fun class)
             (dolist (subclass (subclasses class))
               (res subclass))))
    (res "/")))
