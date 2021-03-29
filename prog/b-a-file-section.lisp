(let ((current-class "/")
      (modes-cycle (list 'move 'remove 'nothing))
      (add-to-bucket #'pass)
      (refresh-files #'pass))
  (labels ((next-mode (mode)
             (fallback (second (member mode modes-cycle :test #'equal))
                       (first modes-cycle)))

           (bucket-section (r c master)
             (let* ((f (frame r c master))
                    (fr (frame 0 0 f))
                    bucket-buttons
                    bucket-urls
                    (mode (first modes-cycle)))
               
               (letrec ((b (button 0 1 f mode #'(lambda () (setf (ltk:text b) (setf mode (next-mode mode)))))))
                 (labels ((new-bucket-button (url origin)
                            (if (find url bucket-urls :key #'car :test #'equal)
                                (warning-box "File already in bucket." "Nope!")
                                (letrec ((index (length bucket-buttons))
                                         (url-description (cons url origin))
                                         (new-button (button index
                                                             0
                                                             fr
                                                             url-description
                                                             #'(lambda ()
                                                                 (action url origin current-class mode)
                                                                 (setf bucket-urls (remove url-description bucket-urls :test #'equal))
                                                                 (ltk:destroy new-button)
                                                                 (funcall refresh-files current-class)))))
                                  (push (cons url origin) bucket-urls)
                                  (push new-button bucket-buttons)))))
                   #'new-bucket-button))))

           (file-section (r c master)
             (let* ((fr (frame r c master))
                    (file-list (frame 0 0 fr))
                    (e (entry 1 0 fr)))
               (button 2 0 fr "Add to this class" #'(lambda ()
                                                      (warn-on-error ("Website error")
                                                        (let ((url (ltk:text e)))
                                                          (add-url url current-class)
                                                          (let ((words (word-count (tokens url))))
                                                            (if (< words *min-word-count*)
                                                                (warning-box (concat "This file is very short! Word count: " words ". It will be added, remove it if you consider this an error.") "Few words!")))
                                                          (funcall refresh-files current-class)))
                                                      (setf (ltk:text e) "")))
               (labels ((refresh (new-class)
                          (setf current-class new-class)
                          (let ((urls (class-urls current-class)))
                            (ltk:destroy file-list)
                            (setf file-list
                                  (scrollable-list 0 0 fr *entries-per-page* urls
                                                   (mapcar #'(lambda (url)
                                                               (lambda () (funcall add-to-bucket url current-class)))
                                                           urls))))))
                 
                 #'refresh))))

    ;; Our ultimate output is merely a function returning the file section refresher & frame
    (defun file-frame (r c master)
      (let ((fr (frame r c master)))
        (setf refresh-files (file-section 0 0 fr))
        (setf add-to-bucket (bucket-section 0 1 fr))
        (values refresh-files fr)))))
