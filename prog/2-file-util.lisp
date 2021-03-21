(defun overwrite-file (path &rest things)
  (ensure-directories-exist path)
  (with-open-file (stream path :direction :output :if-exists :supersede :if-does-not-exist :create)
    (dolist (thing things)
      (print thing stream))))

(defun overwrite-class-file (class file-name &rest things)
  (apply #'overwrite-file (concat (full-path class) file-name) things))

(defun add-url (url class)
  (if (member url (class-urls "/" t) :test #'equal)
      (error (concat "File already in " (location url)))
      (let* ((urls-file (concat (full-path class) "urls"))
             (existing-urls (read-from-file urls-file)))
        (redownload-file url)
        (overwrite-file urls-file (append1 existing-urls url)))))

(defun remove-url (url class)
  (overwrite-class-file class "urls"
                        (remove-if #'(lambda (checked-url)
                                       (equal checked-url url))
                                   (read-from-file (concat (full-path class) "urls")))))

(defun set-weight (class weight)
  (overwrite-class-file class "weight" weight))

;;; MODIFYING FILES
;;;----------------------------------------------------------------------------------------------
;;; READING FILES

(defun read-from-file (path)
  (let ((exists? (directory path)))
    (values (if exists?
                (with-open-file (stream path :direction :input :if-does-not-exist :error)
                  (read stream)))
            exists?)))

(defun read-comment (class)
  (read-from-file (concat (full-path class) "comment")))

;; TBD: Abstractions! (TBD together with corpus export changes)
(defun location (url &optional (class "/"))
  ;; returns the class where the url is to be found
  (if (member url (class-urls class) :test #'equal)
      class
      (let ((sub (find-if #'(lambda (subclass) (member url (class-urls subclass t) :test #'equal))
                          (subclasses class))))
        (if sub
            (location url sub)))))

(defun class-urls (class &optional recursive?)
  (labels ((helper (class)
             (read-from-file (concat (full-path class) "urls"))))
    (append (helper class)
            (if recursive?
                (apply #'append
                       (mapcar #'(lambda (class) (class-urls class t))
                               (subclasses class)))))))

(defun discovered-location (url &optional (class "/"))
  ;; returns the class where the url is to be found
  (if (member url (discovered-urls class) :test #'equal)
      class
      (let ((sub (find-if #'(lambda (subclass) (member url (discovered-urls subclass t) :test #'equal))
                          (subclasses class))))
        (if sub
            (discovered-location url sub)))))

(defun discovered-urls (class &optional recursive?)
  (labels ((helper (class)
             (read-from-file (concat (discovered-path class) "urls"))))
    (append (helper class)
            (if recursive?
                (apply #'append
                       (mapcar #'(lambda (class) (discovered-urls class t))
                               (subclasses class)))))))

(defun get-weight (class)
  (fallback (read-from-file (concat (full-path class) "weight"))
            1))

;;; READING FILES
;;;----------------------------------------------------------------------------------------------
;;; CONVERTORS

(defun simplified-path (path)
  ;; BUG: fails if the classes folder contains "classes" twice in it (remove the whole thing!)
  (let* ((path-parts (cl-strings:split path #\/))
         (varying-part (cdr (member "classes" path-parts :test #'equal))))
    (reduce #'(lambda (a b) (concat a "/" b))
            varying-part
            :initial-value "")))

(defun full-path (simple-path)
  (concat *classes-folder* (subseq simple-path 1)))

(defun discovered-path (simple-path)
  (concat *discovered-folder* simple-path))

;;; CONVERTORS
;;;----------------------------------------------------------------------------------------------
;;; FOLDER NAVIGATION

(defun subclasses (class)
  (mapcar #'simplified-path (mapcar #'namestring (directory (concat (full-path class) "*/")))))

(defun parent-class (class)
  (concat (cl-strings:join (remove-last (remove-last (cl-strings:split class #\/))) :separator "/") "/"))

(defun folder-name (path)
  (second (reverse (cl-strings:split path #\/))))

;;; FOLDER NAVIGATION
;;;----------------------------------------------------------------------------------------------
;;; ALIASES

(defun add-to-aliases (entry aliases)
  (let* ((new-alias (1+ (apply #'max (append1 (list-values aliases) 0))))
         (acc aliases))
    (setf (gethash entry acc) new-alias)
    acc))

(defun file-alias (file-name)
  (gethash file-name (url-aliases)))

(defun domain-alias (domain)
  (gethash domain (domain-aliases)))


(defun url-aliases ()
  (assoc-to-hashtable (read-from-file *aliases-file*)))

(defun domain-aliases ()
  (assoc-to-hashtable (read-from-file *domain-aliases-file*)))


(defun read-core-text (url)
  (read-from-file (concat *core-text-folder* (file-alias url))))

(defun read-text (url)
  (read-from-file (concat *text-folder* (file-alias url))))

(defun read-origin (url)
  (read-from-file (concat *loc-folder* (file-alias url))))

(defun read-html (url)
  (read-from-file (concat *html-folder* (file-alias url))))

(defun read-raw (url)
  (read-from-file (concat *raw-folder* (file-alias url))))

(defun read-domain-urls (domain)
  (read-from-file (concat *domain-lists-folder* (domain-alias domain))))

(defun read-domain-boilerplate (domain)
  (read-from-file (concat *boilerplate-folder* (domain-alias domain))))


(defun add-domain-alias (domain)
  (if (null (domain-alias domain))
      (overwrite-file *domain-aliases-file* (hashtable-to-assoc (add-to-aliases domain (domain-aliases)))))
  (domain-alias domain))

(defun add-alias (url)
  (if (null (file-alias url))
      (overwrite-file *aliases-file* (hashtable-to-assoc (add-to-aliases url (url-aliases)))))
  (file-alias url))

;;; ALIASES
;;;----------------------------------------------------------------------------------------------
;;; DOWNLOADING FILES

(defun redownload (class)
  ;; BUG: Halts if any site has any error. Currently solvable by adding something manually, but pretty bad overall
  ;; Solution: Redownload through GUI, solve errors with error boxes. Keep errors for last so that a minimum of work time gets lost if user is AFK
  (ensure-directories-exist *files-folder*)
  (if (not (directory *aliases-file*))
      (overwrite-file *aliases-file* nil))
  (let ((files (class-urls class)))
    (dolist (file files)
      (redownload-file file))
    (dolist (subclass (subclasses class))
      (redownload subclass)))
  (print (concat "redownloaded " class)))

;; TBD: Rename
(defun redownload-file (file-name)
  (if (not (file-alias file-name))
      (multiple-value-bind (html response-origin)
          (html file-name)
        (let* (                         ;(raw (extract-raw-text html))
               (text (extract-text html))
               (new-alias (add-alias file-name)))
          (overwrite-file (concat *loc-folder* new-alias) response-origin)
          (overwrite-file (concat *html-folder* new-alias) html)
                                        ;(overwrite-file (concat *raw-folder* new-alias) raw)
          (overwrite-file (concat *text-folder* new-alias) text))))
  (file-alias file-name))

;;; DOWNLOADING FILES
;;;----------------------------------------------------------------------------------------------
;;; UTIL

(defun apply-to-all-classes (fun)
  ;; not used in code, but useful to reset weights or stuff
  ;; TBD: Add to a better crawler testing suite
  (labels ((res (class)
             (funcall fun class)
             (dolist (subclass (subclasses class))
               (res subclass))))
    (res "/")))
