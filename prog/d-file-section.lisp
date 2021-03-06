(let ((modes-cycle (list 'move 'remove 'open 'nothing))
      (add-to-bucket #'pass)
      (refresh-files #'pass))
  (labels ((add-url-section (r c master)
             (let* ((fr (frame r c master))
                    (e (described-entry 0 0 fr "URL: ")))
               (button 1 0 fr "Add to this class" #'(lambda ()
                                                      (warn-on-error ("Website error")
                                                        (let ((url (ltk:text e)))
                                                          (add-url url (get-current-class))
                                                          (let ((words (word-count (tokens (read-text url)))))
                                                            (if (< words *min-word-count*)
                                                                (warning-box (concat "This file is very short! Word count: " words ". It will be added, remove it if you consider this an error.") "Few words!")))
                                                          (funcall refresh-files)))
                                                      (setf (ltk:text e) "")))
               fr))

           (add-text-section (r c master)
             (let* ((fr (frame r c master))
                    (e (described-entry 0 0 fr "Manual file name: "))
                    (tx (text 1 0 fr "" 5 15)))
               (button 2 0 fr "Add this text here" #'(lambda ()
                                                       (let ((url (concat "manual:" (ltk:text e))))
                                                         (add-manual-file url (ltk:text tx))
                                                         (add-manual-url url (get-current-class))
                                                         (funcall refresh-files))
                                                       (setf (ltk:text tx) "")
                                                       (setf (ltk:text e) "")))
               (choose-file-button 3 0 fr "Load text from file" tx)
               fr))

           (file-section (r c master)
             (let* ((fr (frame r c master))
                    (file-list (frame 0 0 fr)))
               (add-url-section 1 0 fr)
               (add-text-section 2 0 fr)
               #'(lambda ()
                   (let* ((editable (class-urls (get-current-class)))
                          (imported (imported-urls (get-current-class)))
                          (urls (append editable imported)))
                     (ltk:destroy file-list)
                     (setf file-list
                           (scrollable-list 0 0 fr *entries-per-page* urls
                                            (mapcar #'(lambda (url)
                                                        (lambda () (funcall add-to-bucket url (get-current-class))))
                                                    urls)
                                            (append (make-list (length editable) :initial-element nil)
                                                    (make-list (length imported) :initial-element t)))))))))

    ;; Our ultimate output is merely a function returning the file section refresher & frame
    (defun file-frame (r c master)
      (let ((fr (frame r c master)))
        (setf refresh-files (file-section 0 0 fr))
        (setf add-to-bucket (bucket-section 0 1 fr #'(lambda (url origin class mode)
                                                       (action url origin class mode)
                                                       (funcall refresh-files))
                                            modes-cycle #'cons))
        (values refresh-files fr)))))
