(defun blind-check (url folder)
  (let* ((corp (downloaded-link-corpus url))
         (sibling-folders (subfolders (parent-folder folder)))
         (simple-path-scores (map-to-hash #'cdr
                                          (list-hashes (scores (list-keys corp)
                                                               sibling-folders
                                                               (map-to-hash #'(lambda (option)
                                                                                (if (equal (simplified-path option)
                                                                                           (simplified-path folder))
                                                                                    (add-hashtable-corpuses (get-recursive-corpus option)
                                                                                                            (scale-corpus corp -1))
                                                                                    (get-recursive-corpus option)))
                                                                            sibling-folders)
                                                               (map-to-hash #'get-word-count sibling-folders)))
                                          :key-fun (compose #'simplified-path #'car))))
    (gethash (simplified-path folder) simple-path-scores)))

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
