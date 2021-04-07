(defun overwrite-file (path &rest things)
  (ensure-directories-exist path)
  (with-open-file (stream path :direction :output :if-exists :supersede :if-does-not-exist :create)
    (dolist (thing things)
      (print thing stream))))

(defun append-to-file (path txt)
  (with-open-file (stream path :direction :output :if-exists :append :if-does-not-exist :create)
    (print txt stream)))

(defun read-from-file (path)
  (let ((exists? (directory path)))
    (values (if exists?
                (with-open-file (stream path :direction :input :if-does-not-exist :error)
                  (read stream)))
            exists?)))

;;; BASIC I/O
;;;----------------------------------------------------------------------------------------------
;;; FOLDER NAVIGATION

(defun parent-class (class)
  (if (not (equal class "/"))
      (concat (cl-strings:join (remove-last (remove-last (cl-strings:split class #\/))) :separator "/") "/")))

(defun folder-name (path)
  (second (reverse (cl-strings:split path #\/))))

(defun path-after-renaming (class new-name)
  (concat (parent-class class) new-name "/"))

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

(defun manual? (url)
  (equal (quri:uri-scheme (quri:uri url))
         "manual"))

(defun add-manual-file (url content)
  (if (not (file-alias url))
      (let* ((text (basic-text content))
             (new-alias (add-alias url)))
        (overwrite-file (concat *loc-folder* new-alias) url)
        (overwrite-file (concat *html-folder* new-alias) content)
        (overwrite-file (concat *text-folder* new-alias) text)))
  (file-alias url))
