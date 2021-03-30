(let ((modes-cycle (list 'move 'remove 'nothing))
      (add-to-bucket #'pass)
      (refresh-files #'pass))
  (labels ((file-section (r c master)
             (let* ((fr (frame r c master))
                    (file-list (frame 0 0 fr))
                    (e (entry 1 0 fr)))
               (button 2 0 fr "Add to this class" #'(lambda ()
                                                      (warn-on-error ("Website error")
                                                        (let ((url (ltk:text e)))
                                                          (add-url url (get-current-class))
                                                          (let ((words (word-count (tokens (read-text url)))))
                                                            (if (< words *min-word-count*)
                                                                (warning-box (concat "This file is very short! Word count: " words ". It will be added, remove it if you consider this an error.") "Few words!")))
                                                          (funcall refresh-files)))
                                                      (setf (ltk:text e) "")))
               (labels ((refresh ()
                          (let ((urls (class-urls (get-current-class))))
                            (ltk:destroy file-list)
                            (setf file-list
                                  (scrollable-list 0 0 fr *entries-per-page* urls
                                                   (mapcar #'(lambda (url)
                                                               (lambda () (funcall add-to-bucket url (get-current-class))))
                                                           urls))))))
                 
                 #'refresh))))

    ;; Our ultimate output is merely a function returning the file section refresher & frame
    (defun file-frame (r c master)
      (let ((fr (frame r c master)))
        (setf refresh-files (file-section 0 0 fr))
        (setf add-to-bucket (bucket-section 0 1 fr #'action refresh-files modes-cycle #'cons))
        (values refresh-files fr)))))