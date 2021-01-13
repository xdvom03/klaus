(defun integrity (folder)
  (let ((links (recursive-links folder)))
    (my-round (/ (apply #'+ (mapcar #'(lambda (link)
                                        (blind-check link folder))
                                    links))
                 (length links)))))

(defun recursive-links (folder)
  (append (class-links folder)
          (apply #'append
                 (mapcar #'recursive-links
                         (subfolders folder)))))
