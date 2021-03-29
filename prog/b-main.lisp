#|
TBD: There are tons of built in useful sequence functions (positions, search, find...). Check if they could simplify!
Naming convention: 'class' is simplified path, 'folder' is actual folder.
|#

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
    (nothing
     nil)))

;;; FILES & BUCKET
;;;----------------------------------------------------------------------------------------------
;;; THE TOTALITY

(let ((refresh-files #'pass)
      (refresh-classes #'pass)
      (current-class "/")
      (W nil))
  ;; shared environment variables for parts of GUI

  (defun get-current-class ()
    current-class)

  (defun set-current-class (new-value)
    (setf current-class new-value))

  (defun rebuild-frame (r c master)
    ;; DOES NOT refresh imports. This means imports can only be added before program launch.
    (let ((fr (frame r c master)))
      (button 0 0 fr "SAVE ALL CHANGES" #'(lambda ()
                                            (let ((timer (get-internal-real-time)))
                                              (build-core-text-database)
                                              (rebuild-corpus)
                                              (show-time timer "Rebuilt the corpus.")
                                              (funcall refresh-classes)
                                              (save-corpora)
                                              (save-config)
                                              (refresh-imports))))
      fr))

  (defun db-window ()
    (setf W (window "/"))
    (ltk:on-close W #'(lambda () (ltk:destroy ltk:*tk*)))
    (rebuild-frame 0 2 W)
    (button 0 3 W "Go to classifier" #'(lambda ()
                                         (ltk:destroy W)
                                         (classifier-window)))
    (multiple-value-bind (classes-refresher set-file-refresher)
        (class-section 0 0 W)
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

;; current class = possible problem
