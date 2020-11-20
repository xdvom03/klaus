(defun class-links (class)
  (read-from-file (concat class "links")))

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

(defun url-text (url)
  ;; Fetches text of an url
  ;; Placeholder for now until the MAJOR REFACTORING
  ;; Duplicate-ish of extract-text
  (let* ((raw (safe-fetch-html url))
         (safe (make-safe (remove-diacritics (string-downcase raw))))
         (content (remove-punctuation (remove-tags (remove-enclosed (remove-enclosed safe "<style" "</style>") "<script" "</script>")))))
    content))
