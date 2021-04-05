(let ((discovered-urls (let ((acc (ht)))
                         (apply-to-all-classes #'(lambda (class)
                                                   (if (not (read-tentative class))
                                                       (setf (gethash class acc)
                                                             nil))))
                         acc)))
  (defun save-discovered ()
    (overwrite-file *discovered-file* (hashtable-to-assoc discovered-urls)))
  
  (defun discover (url place)
    (let ((existing-urls (gethash place discovered-urls)))
      (if (not (member url existing-urls :test #'equal))
          (setf (gethash place discovered-urls)
                (append1 existing-urls
                         url)))
      place)))

(defun pathlist (path)
  (remove-if #'(lambda (class-name)
                 (equal class-name ""))
             (cl-strings:split path #\/)))

(defun zoombot-place-value (place target)
  ;; the value of the target, disregarding the non-integer portion from final probability
  (let* ((overlap-length (fallback (mismatch (pathlist place)
                                             (pathlist target)
                                             :test #'equal)
                                   (length (pathlist place))))
         (final-folder (concat "/"
                               (cl-strings:join (up-to-n-first (1+ overlap-length)
                                                               (pathlist target))
                                                :separator "/")
                               "/")))
    (values overlap-length
            final-folder)))

(defun zoombot-vocab-value (vocab target)
  (let ((place (place-vocab vocab)))
    (multiple-value-bind (base-score final-folder)
        (zoombot-place-value place target)
      (values (+ base-score
                 (prob vocab
                       final-folder))
              place))))

(defun next-link (current-url visited-urls target same-domain?)
  ;; all scoring happens here
  ;; holder function for cache of downloaded html
  (let ((htmls (ht))
        (locs (ht))
        (texts (ht)))
    (labels ((save-url-data (url)
               ;; analogous to redownload-url
               (if (not (gethash url htmls))
                   (multiple-value-bind (html response-origin)
                       (html url)
                     (let ((text (extract-text html)))
                       (setf (gethash url locs) response-origin)
                       (setf (gethash url htmls) html)
                       (setf (gethash response-origin htmls) html)
                       (setf (gethash url texts) text)
                       (setf (gethash response-origin texts) text)))))
             (simplified-filter-urls (urls domain visited-urls visited-domains same-domain?)
               ;; turns a list of link targets into a list of URLs ready for classification, scoring, & saving
               (terpri)
               (let* ((urls-1 (remove-duplicates urls :test #'equal))
                      (urls-2 (remove-if #'(lambda (url)
                                             (or (not (valid-scheme? url))
                                                 (member (extension url)
                                                         *forbidden-extensions*
                                                         :test #'equal)))
                                         urls-1))
                      (urls-3 (remove-if #'(lambda (url) (find url visited-urls :test #'equivalent-urls))
                                         urls-2))
                      (urls-4 (remove-if-not (if same-domain?
                                                 #'(lambda (url)
                                                     (equal (find-domain url)
                                                            domain))
                                                 #'(lambda (url)
                                                     (not (find (find-domain url)
                                                                visited-domains :test #'equal))))
                                             urls-3)))
                 urls-4))
             (filter-urls (urls domain visited-urls visited-domains same-domain?)
               ;; turns a list of link targets into a list of URLs ready for classification, scoring, & saving
               (terpri)
               (let* ((urls-1 (simplified-filter-urls urls domain visited-urls visited-domains same-domain?))
                      (urls-2 (up-to-n-first *link-cap*
                                             (shuffle urls-1)))
                      (urls-3 (remove-if-not #'(lambda (url)
                                                 (if (url-allowed? *crawler-name* url)
                                                     (handler-case (progn
                                                                     (save-url-data url)
                                                                     (let* ((text (gethash url texts))
                                                                            (vocab (wordlist text)))
                                                                       (if (> (length vocab) *min-word-count*)
                                                                           (if (> (comprehensible? vocab) *min-word-comprehensibility*)
                                                                               (princ "O")
                                                                               (progn
                                                                                 (princ "?")
                                                                                 nil))
                                                                           (progn
                                                                             (princ "s")
                                                                             nil))))
                                                       ;; Any error will cause the system to refuse the link (typically because the site is unreachable or contains invalid content)
                                                       (error (err-text)
                                                         (declare (ignore err-text))
                                                         (princ "e")
                                                         nil))
                                                     (princ "r")))
                                             urls-2)))
                 urls-3)))
      (handler-case (save-url-data current-url)
        (error (err-text)
          (declare (ignore err-text))
          (print (concat "Current url (" current-url ") is broken"))
          nil))
      
      (let* ((domain (find-domain current-url))
             (urls (filter-urls (vetted-links current-url (gethash current-url htmls))
                                domain visited-urls (mapcar #'find-domain visited-urls) same-domain?))
             (url-options (simplified-filter-urls (mapcar #'(lambda (url)
                                                              (gethash url locs))
                                                          urls)
                                                  domain visited-urls (mapcar #'find-domain visited-urls) same-domain?))
             (link-scores (map-to-hash #'(lambda (url) (multiple-value-bind (score place)
                                                           (zoombot-vocab-value (remove-duplicates (wordlist (gethash url texts))) target)
                                                         (discover url place) ;; this is where all discovering happens
                                                         score))
                                       url-options)))
        (if url-options
            (best-element (reverse url-options)
                          #'> #'(lambda (url) (gethash url link-scores))))))))

(defun domain-links (seed target count)
  ;; visits links in succession (different from interdomain, may change) to get wider site coverage
  ;; if it finds no valid links, it keeps trying again (maybe getting other links)
  ;; returns do not include the seed
  (let ((scores (ht))
        (visited nil)
        (current-url seed))
    (dotimes (i count)
      (save-discovered)
      (multiple-value-bind (url score)
          (next-link current-url visited target t)
        (if url
            (progn
              (print url)
              (print "SCORE")
              (print score)
              (append-to-file *scores-file*
                              (concat (my-round score) " " url))
              (setf (gethash url scores) score)
              (setf current-url url)
              (push url visited)))))
    (values (reverse visited) scores)))

(defun zoombot (seed domains per-domain target progress-bar)
  ;; the seed does not get scored
  (setf *random-state* (make-random-state t))
  (let* ((visited-urls nil)
         (followed-urls nil)
         (url-scores (ht))
         (timer (get-internal-real-time)))

    (labels ((score-url (url score)
               (setf (gethash url url-scores)
                     score))

             (traverse-domain (new-url)
               (multiple-value-bind (domain-links domain-scores)
                   (domain-links new-url target per-domain)
                 (dolist (link domain-links)
                   (push link visited-urls)
                   (score-url link (gethash link domain-scores)))))

             (enter-new-domain (url)
               (multiple-value-bind (chosen-url chosen-score)
                   (next-link url visited-urls target nil)
                 (when chosen-url
                   (print (concat "New domain opened with " chosen-url " with score " chosen-score))
                   (setf (gethash chosen-url url-scores)
                         chosen-score)
                   (traverse-domain chosen-url)
                   t))))

      (traverse-domain seed)
      (do ((i 0))
          ((>= i domains))
        (gc :full t)
        (print "NEW DOMAIN. Options:")
        (setf (ltk-mw:percent progress-bar) (* 100 (print (/ (1+ i) domains))))
        (let* ((options (print (set-difference visited-urls followed-urls)))
               (starting-url (if options
                                 (best-element options
                                               #'> #'(lambda (url) (gethash url url-scores)))
                                 (error "Ran out of URLs in the queue"))))
          (push starting-url followed-urls)
          (if (enter-new-domain starting-url)
              (incf i)))))
    (show-time timer "Crawl complete.")
    (print (reverse visited-urls))))





















(defun crawler-window (&optional (initial-target ""))
  (let ((W (window "Botelaire welcomes you, human being!")))
    (ltk:on-close W #'(lambda ()
                        (ltk:destroy ltk:*tk*)))
    (let ((pb (progress-bar 3 0 W)))
      (let* ((f (frame 0 0 W))
             (e1 (entry 0 1 f))
             (e2 (entry 1 1 f))
             (e3 (entry 2 1 f))
             (e4 (entry 3 1 f initial-target)))
        
        (label 0 0 f "Starting URL")
        (label 1 0 f "Domains")
        (label 2 0 f "Steps per domain")
        (label 3 0 f "Target class")
        
        (button 4 0 f "Start Zoombot" #'(lambda ()
                                          (zoombot (ltk:text e1)
                                                   (read-from-string (ltk:text e2))
                                                   (read-from-string (ltk:text e3))
                                                   (ltk:text e4)
                                                   pb))))
      (button 1 0 W "Go to classifier" #'(lambda ()
                                           (ltk:destroy W)
                                           (classifier-window)))
      (button 2 0 W "Go to database" #'(lambda ()
                                         (ltk:destroy W)
                                         (db-window))))))

(defun crawler ()
  (ltk:with-ltk ()
    (ltk:withdraw ltk:*tk*)
    (crawler-window)))
