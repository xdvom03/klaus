(defun parent-button-state (class)
  (if (equal class
             "/")
      :disabled
      :normal))

(defun action (url origin class mode)
  (case mode
    (move
     ;; needlessly checks for continued existence of file
     (remove-url url origin)
     (add-url url class))
    (remove
     (remove-url url origin))
    (open
     (open-url url))
    (nothing
     nil)))

;;; FILES & BUCKET
;;;----------------------------------------------------------------------------------------------
;;; THE TOTALITY

(defun save-all ()
  (build-core-text-database)
  (rebuild-corpus)
  (save-corpora)
  (save-config)
  (save-imports)
  (refresh-imports) ; loads any new import files
  )

(let ((refresh-files #'pass)
      (refresh-classes #'pass)
      (current-class "/")
      (W nil))

  (defun get-current-class ()
    current-class)

  (defun set-current-class (new-value)
    (setf current-class new-value))

  (defun rebuild-frame (r c master)
    (let ((fr (frame r c master)))
      (button 0 0 fr "Save all changes" #'(lambda ()
                                            (let ((timer (get-internal-real-time)))
                                              (save-all)
                                              (funcall refresh-classes)
                                              (show-time timer "Saved."))))
      fr))

  (defun db-window ()
    (setf W (window "Klaus database"))
    (set-current-class "/")
    (ltk:on-close W #'(lambda ()
                        (if (choice-box "Save before closing?" "Bye!")
                            (save-all))
                        (ltk:destroy ltk:*tk*)))
    (rebuild-frame 0 2 W)
    (button 2 0 W "Go to classifier" #'(lambda ()
                                         (ltk:destroy W)
                                         (classifier-window)))
    (button 3 0 W "Go to crawler" #'(lambda ()
                                      (ltk:destroy W)
                                      (crawler-window current-class)))
    (multiple-value-bind (classes-refresher set-file-refresher)
        (class-section 0 0 W)
      (setf refresh-files (file-frame 1 0 W))
      (funcall set-file-refresher refresh-files)
      (funcall refresh-files)
      (setf refresh-classes classes-refresher)
      (funcall refresh-classes)
      (setf refresh-classes classes-refresher)))

  (defun crawl-results-window ()
    (setf W (window "Crawl results"))
    (ltk:on-close W #'(lambda ()
                        (if (choice-box "Save your edits to the crawl results file before closing?" "Bye!")
                            (save-edited-discovered))
                        (ltk:destroy ltk:*tk*)))
    (multiple-value-bind (classes-refresher set-file-refresher)
        (class-section 0 0 W t)
      (setf refresh-files (file-frame 1 0 W))
      (funcall set-file-refresher refresh-files)
      (funcall refresh-files)
      (setf refresh-classes classes-refresher)
      (funcall refresh-classes)
      (setf refresh-classes classes-refresher))))

(defun db ()
  (ltk:with-ltk ()
    (ltk:withdraw ltk:*tk*)
    (db-window)))

(defun display-crawl-results ()
  (ltk:with-ltk ()
    (ltk:withdraw ltk:*tk*)
    (crawl-results-window)))
