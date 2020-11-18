(defun class-links (class)
  (with-open-file (stream (concat class "links"))
    (read stream)))

(defun redownload (folder)
  (let ((files (class-links folder)))
    (dolist (file files)
      (redownload-file file))
    (dolist (subfolder (subfolders folder))
      (redownload subfolder))))

(defun redownload-file (file-name)
  (if (not (file-alias file-name))
      (add-alias file-name (1+ (apply #'max (append1 (used-aliases) 0))))
      ; TBD: maybe check for newer version?
      )
  (file-alias file-name))

(defun add-alias (name alias)
  (let ((aliases (aliases)))
    (with-open-file (stream *aliases-file* :direction :output :if-exists :supersede)
      (print (append1 aliases (cons name alias)) stream)))
  (with-open-file (stream (concat *files-folder* alias) :direction :output :if-exists :supersede :if-does-not-exist :create)
    (print (safe-fetch-html name) stream) ;; TBD: :( -> :)
    ))

(defun file-alias (file-name)
  (gethash file-name (corpus-hashtable (aliases))))

(defun aliases ()
  (with-open-file (stream *aliases-file*)
    (read stream)))

(defun used-aliases ()
  (mapcar #'cdr (aliases)))

(defun file-content (file-name)
  (with-open-file (stream (print (concat *files-folder* (file-alias file-name))))
    (read stream)))

(defun url-text (url)
  ;; Fetches text of an url
  ;; Placeholder for now until the MAJOR REFACTORING
  (let* ((raw (safe-fetch-html url))
         (safe (make-safe (remove-diacritics (string-downcase raw))))
         (content (remove-whitespace (remove-tags (remove-enclosed (remove-enclosed safe "<style" "</style>") "<script" "</script>")))))
    content))
