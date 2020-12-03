(defun add-to-history (url)
  ;; TBD: Generalise in case we need more appendable files.
  (with-open-file (stream *history-file* :direction :output :if-exists :append :if-does-not-exist :create)
    (print url stream)))

(defun history ()
  (reverse (mapcar #'read-from-string (file-lines *history-file*))))

(defun remove-from-history (index)
  (log-print "Removing " (nth index (history)) " from history.")
  (let ((hst (history)))
    (apply #'overwrite-file *history-temp-file* (reverse (remove-nth index hst)))
    (delete-file *history-file*)
    (rename-file *history-temp-file* *history-rename*)))
