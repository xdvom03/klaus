(defun overwrite-file (path &rest things)
  (with-open-file (stream path :direction :output :if-exists :supersede :if-does-not-exist :create)
    (dolist (thing things)
      (print thing stream))))

(defun file-lines (file)
  ;; UIOP for some reason always reads a blank first line
  ;; List of string lines
  (cdr (uiop:read-file-lines file)))

(defun read-from-file (path)
  (with-open-file (stream path :direction :input :if-does-not-exist nil)
    (read stream)))

;;; FILE READING
;;;----------------------------------------------------------------------------------------------
;;; FOLDER NAVIGATION

(defun directory* (folder)
  (mapcar #'namestring (directory (concat folder "*"))))

(defun read-comment (folder)
  (if (directory (concat folder "comment"))
      (read-from-file (concat folder "comment"))
      (set-comment "" folder)))

(defun set-comment (text folder)
  (overwrite-file (concat folder "comment") text))

(defun subfolders (folder)
  (remove-if-not #'folder? (directory* folder)))

(defun class-links (class)
  (read-from-file (concat class "links")))

(defun simplified-path (path)
  (let* ((path-parts (split path (char "/" 0)))
         (varying-part (cdr (member "classes" path-parts :test #'equal))))
    (reduce #'(lambda (a b) (concat a "/" b))
            ;; Prepend an empty string in case of the home folder - reducing NIL it has no idea what the base state is.
            (cons "" varying-part))))

(defun parent-folder (folder-path)
  (do ((i (- (length folder-path) 2) (1- i)))
      ((slash? (char folder-path i))
       (subseq folder-path 0 (1+ i)))))

(defun file-name (path folder?)
  (do ((i (if folder?
              (- (length path) 2)
              (1- (length path)))
          (1- i)))
      ((slash? (char path i))
       (subseq path (1+ i)))))

(defun redownload (folder)
  (let ((files (class-links folder)))
    (dolist (file files)
      (redownload-file file))
    (dolist (subfolder (subfolders folder))
      (redownload subfolder)))
  (print (concat "redownloaded " folder)))

(defun redownload-file (file-name)
  (if (not (file-alias file-name))
      (add-alias file-name (1+ (apply #'max (append1 (used-aliases) 0))))
      ;; TBD: maybe check for newer version?
      )
  (file-alias file-name))

(defun add-alias (name alias)
  (let ((aliases (aliases)))
    (overwrite-file *aliases-file* (append1 aliases (cons name alias)))
    (overwrite-file (concat *files-folder* alias) (safe-fetch-html name))))

(defun file-alias (file-name)
  (gethash file-name (corpus-hashtable (aliases))))

(defun aliases ()
  (read-from-file *aliases-file*))

(defun used-aliases ()
  (mapcar #'cdr (aliases)))

(defun file-content (file-name)
  (read-from-file (concat *files-folder* (file-alias file-name))))
