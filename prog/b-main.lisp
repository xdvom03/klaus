#|
PROBLEM: A few tiny folders can mess up the confidence for the whole superfolder. Add more data.
TBD: Toggle adding into history
TBD: Speed up
TBD: Load title for links
TBD: Autoclose explainers when a thing is classed.
TBD: Consider a category an average of its subcategories, not files. This would help protect against availability bias (say, Lifehack taking over trash) and make the system more semantic.

What do we do with <noscript>? It caused c2wiki to class as boilerplate, but I think there's no good reason to exclude it. Made irrelevant by C2wiki depending on Javascript, and thus not being downloadable.

Some sites use scripts to deliver boilerplate. While this is not a problem for classification (just ignore them or input manually), it might mess up the crawler.
|#

;;; NORMAL STUFFS
;;;----------------------------------------------------------------------------------------------
;;; GUI

(defparameter *modes-cycle* (list 'add 'remove 'move 'nothing))

(defun next-mode (mode)
  (fallback (second (member mode *modes-cycle* :test #'equal))
            (first *modes-cycle*)))

(defun folder-name (path)
  (second (reverse (split path #\/))))

(defun remove-link (link folder)
  (let ((file (concat folder "links")))
    (overwrite-file file
                    (remove-if #'(lambda (checked-url)
                                   (equal checked-url link))
                               (read-from-file file)))))

(defun add-link (link class)
  ;; Avoiding double insert
  (if (not (member link (class-links class) :test #'equal))
      (let* ((links-file (concat class "links"))
             (existing-links (read-from-file links-file)))
        (redownload-file link)
        (overwrite-file links-file (append1 existing-links link)))
      (warning-box "File already in folder." "nice try")))

(defun action (link origin class mode)
  (case mode
    (add
     (add-link link class))
    (move
     (remove-link link origin)
     (add-link link class))
    (remove
     (remove-link link origin))
    (nothing
     nil)))

(defun copy (var)
  ;; Rebinds a variable to pass it by value
  (let ((acc var))
    acc))

(defun db ()
  #|
  TBD: Abstract all the GUI stuff elsewhere
  TBD: This is a mess. Read up on C2 Wiki GUI design, see if this can be done more functionally.
  |#
  (ltk:with-ltk ()
    (ltk:withdraw ltk:*tk*)
    (let* ((W (window "klaus")))
      (ltk:on-close W #'(lambda () (ltk:destroy ltk:*tk*)))
      (let* ((fr (frame 0 0 W))
             (current-folder *classes-folder*)
             ;; Conses: (link . original-path)
             (bucket-links nil)
             (subfolder-buttons nil)
             (bucket-buttons nil)
             
             (folder-frame (frame 0 0 fr))
             (file-frame (frame 1 0 W))
             (comment-frame (frame 0 1 fr))
             (bucket-frame (frame 0 2 fr))
             (options-frame (frame 0 3 fr))

             ;; variable stuff
             (old-comment (read-comment *classes-folder*))
             (e (entry 1 0 bucket-frame))
             (tex (text 0 0 comment-frame "" 10 20 "NotoMono 10"))
             (file-list (frame 0 1 file-frame))
             parent-button

             ;; settings
             (mode (first *modes-cycle*))
             (show-file-counts? nil)
             (show-word-counts? nil)
             (show-word-details? nil)
             (show-files? t)

             (detail-frame (frame 5 0 options-frame))
             (e2 (entry 0 1 detail-frame))
             (files-label (if show-files? (label 0 0 file-frame "FILES"))))
        (labels ((folder-description (folder word)
                   (concat (folder-name folder)
                           (if (or show-file-counts?
                                   show-word-counts?)
                               ": "
                               "")
                           (if show-word-counts?
                               (concat (get-word-count folder) " words ")
                               "")
                           (if show-file-counts?
                               (concat (get-file-count folder) " files ")
                               "")
                           (if show-word-details?
                               (concat " word count: "
                                       (occurrences word (get-recursive-corpus folder))
                                       ", out of "
                                       (get-word-count folder)
                                       " words in total. Portion: "
                                       (my-round (* 10000 (/ (occurrences word (get-recursive-corpus folder))
                                                             (get-word-count folder))))
                                       "‱")
                               "")))

                 (destroy-widgets (list)
                   (dolist (w list)
                     (ltk:destroy w)))

                 (new-bucket-button (link origin)
                   (let ((index (length bucket-buttons)))
                     (button (+ 3 index)
                             0
                             bucket-frame
                             (cons link (simplified-path origin))
                             #'(lambda ()
                                 (action link origin current-folder mode)
                                 (setf bucket-links (remove-nth index bucket-links))
                                 (redraw-bucket)
                                 (redraw-files)))))
                 
                 (add-to-bucket (link origin)
                   (setf bucket-links (append1 bucket-links (cons link origin)))
                   (push (new-bucket-button link origin)
                         bucket-buttons))

                 (change-folder-confirmed (new-path)
                   (setf current-folder new-path)
                   (setf old-comment (read-comment new-path))
                   (ltk:configure parent-button :state (if (equal (simplified-path new-path)
                                                                  (simplified-path *classes-folder*))
                                                           :disabled
                                                           :normal))
                   (setf (ltk:text tex) (read-comment current-folder))
                   (ltk:wm-title W (simplified-path current-folder))
                   (redraw-subfolders)
                   (redraw-files))

                 (change-folder (new-path)
                   (if (equal old-comment
                              (read-text tex))
                       (change-folder-confirmed new-path)
                       (let ((warning-button nil))
                         (setf warning-button (button 2 0 comment-frame "Change folder despite unsaved comment" #'(lambda ()
                                                                                                                    (change-folder-confirmed new-path)
                                                                                                                    (ltk:destroy warning-button)))))))

                 (redraw-subfolders ()
                   (destroy-widgets subfolder-buttons)
                   (setf subfolder-buttons nil)
                   (let ((subfolders (subfolders current-folder)))
                     (let ((counter 1))
                       (dolist (i subfolders)
                         (incf counter)
                         (push (button counter
                                       0
                                       folder-frame
                                       (folder-description i (intern (ltk:text e2)))
                                       #'(lambda ()
                                           (change-folder i)))
                               subfolder-buttons)))))

                 (redraw-files ()
                   (let ((links (class-links current-folder)))
                     (ltk:destroy file-list)
                     (if show-files?
                         (setf file-list
                               (scrollable-list 1 0 file-frame *entries-per-page* links (mapcar #'(lambda (link) (lambda () (add-to-bucket link current-folder))) links))))))
                 
                 (redraw-bucket ()
                   (destroy-widgets bucket-buttons)
                   (setf bucket-buttons nil)
                   (dolist (link bucket-links)
                     (push (new-bucket-button (car link) (cdr link))
                           bucket-buttons))))
          
          (letrec ((b (button 0 0 options-frame mode #'(lambda () (setf (ltk:text b) (setf mode (next-mode mode))))))
                   (ch (checkbox 1 0 options-frame "Show word counts?" #'(lambda (a)
                                                                           (declare (ignore a))
                                                                           (setf show-word-counts? (ltk:value ch))
                                                                           (redraw-subfolders))))
                   (ch2 (checkbox 2 0 options-frame "Show file counts?" #'(lambda (a)
                                                                            (declare (ignore a))
                                                                            (setf show-file-counts? (ltk:value ch2))
                                                                            (redraw-subfolders))))
                   (b2 (button 3 0 options-frame "Rebuild corpus." #'rebuild-corpus))
                   (ch3 (checkbox 0 0 detail-frame "Word details?" #'(lambda (a)
                                                                       (declare (ignore a))
                                                                       (setf show-word-details? (ltk:value ch3))
                                                                       (redraw-subfolders))))
                   (placeholder-widget nil)
                   (ch4 (checkbox 1 0 detail-frame "Show files?" #'(lambda (a)
                                                                     (declare (ignore a))
                                                                     (setf show-files? (ltk:value ch4))
                                                                     (if (and placeholder-widget show-files?)
                                                                         (ltk:destroy placeholder-widget)
                                                                         (setf placeholder-widget (empty-widget 0 0 file-frame)))
                                                                     (if show-files?
                                                                         (setf files-label (label 0 0 file-frame "FILES"))
                                                                         (ltk:destroy files-label))
                                                                     (redraw-files))))
                   (e3 (entry 6 0 options-frame)))
            (button 7 0 options-frame "Create folder" #'(lambda ()
                                                          (let ((txt (ltk:text e3)))
                                                            (if (equal txt "")
                                                                (warning-box "Empty folder names are a BAD IDEA." "NOPE")
                                                                (progn
                                                                  (ensure-directories-exist (concat current-folder txt "/"))
                                                                  (overwrite-file (concat current-folder txt "/links") nil)
                                                                  (overwrite-file (concat current-folder txt "/comments") "")
                                                                  (rebuild-corpus (concat current-folder txt "/"))
                                                                  (setf (ltk:text e3) "")
                                                                  (change-folder (concat current-folder txt "/")))))))
            (setf (ltk:value ch) show-word-counts?)
            (setf (ltk:value ch2) show-file-counts?)
            (setf (ltk:value ch3) show-word-details?)
            (setf (ltk:value ch4) show-files?))
          (label 0 0 bucket-frame "BUCKET")
          (button 2 0 bucket-frame "Add to bucket" #'(lambda ()
                                                       (add-to-bucket (ltk:text e) nil)
                                                       (setf (ltk:text e) "")))
          (label 0 0 folder-frame "FOLDERS")    
          (button 1 0 comment-frame "Save comment" #'(lambda ()
                                                       (setf old-comment (read-text tex))
                                                       (overwrite-file (concat current-folder "comment") (read-text tex))))
          (setf parent-button (button 1 0 folder-frame ".." #'(lambda () (change-folder (parent-folder current-folder)))))
          (change-folder-confirmed current-folder)
          (redraw-files)
          fr)))))

;; TBD: Fix the relative/absolute folder bug/ugliness
;; TBD: Show blind checks in the regular interface
