#|
TBD: Properly cut off trailing section hashtags and other useless link fluff
TBD: Properly cache link scores, profile
Not using a robots.txt library because is has no license.
TBD: Cache robots.txt (or a sensible format thereof)
BUG: Can get stuck on: "Open too many files" or scoring "http://lightspeed.sourceforge.net/". Make it fail gracefully in these cases, releasing the queue AND acc with each major visited file (not just scored, but actually taken).

Sometimes gets stuck on a random URL for seemingly no reason. TBD: Do time requests, screw it after half a minute.

Crawl 40 from:

"https://www.waitbutwhy.com"
"https://en.wikipedia.org" (got stuck)
"https://www.lifehack.org/891214/happiness-book"
"https://www.jscc.edu/academics/programs/writing-center/writing-resources/five-paragraph-essay.html"
|#

(defun prob (vocab class)
  (let* ((sibling-classes (subclasses (parent-class class)))
         (path-scores (map-to-hash #'cdr
                                   (list-hashes (scores vocab
                                                        sibling-classes
                                                        (map-to-hash #'get-recursive-corpus sibling-classes)
                                                        (map-to-hash #'get-word-count sibling-classes)
                                                        nil))
                                   :key-fun #'car)))
    (gethash class path-scores)))

(defun link-score (url target-class)
  (let ((path "/")
        (vocab (remove-duplicates (tokens (url-text url)) :test #'equal))
        (acc 0)
        (prob 1))
    (dolist (frag (split (subseq target-class 1) #\/))
      (if (not (equal frag ""))
          (progn
            (setf path (concat path frag "/"))
            (setf prob (* prob (prob vocab path)))
            (incf acc prob))))
    prob))

(defun pick-from-queue (queue)
  (car (first (sort (copy-seq queue) #'> :key #'cdr))))

(defun render-wikipedia (link)
  (if (and (or (equal (find-domain link) "https://en.wikipedia.org")
               (equal (find-domain link) "http://en.wikipedia.org"))
           (not (search "action=render" link)))
      (concat link "?action=render")
      link))

(defun core-domain (domain)
  ;; Have to deal with trailing dot
  (second (remove-if #'(lambda (part) (equal part ""))
                     (reverse (split (last1 (split domain #\/)) #\.)))))

(defun append-to-file (path txt)
  (with-open-file (stream path :direction :output :if-exists :append :if-does-not-exist :create)
    (print txt stream)))

(defun wanderbot (seed queue-size page-count target)
  (let ((queue (list (cons seed 0)))
        (acc nil)
        (visited-domains nil)
        (results nil))
    (dotimes (i page-count)
      (print "QUEUE:")
      (print (sort (copy-seq queue) #'> :key #'cdr))
      ;;(print "DOMAINS:")
      ;;(print visited-domains)
      ;;(print "VISITED:")
      ;;(print acc)
      ;;(print "RESULTS:")
      ;;(print results)
      (print (concat "checking " (pick-from-queue queue) ": "))
      (let* ((best-url (pick-from-queue queue))
             (raw-links (filter-links (vetted-links best-url)))
             (links (mapcar #'render-wikipedia
                            (remove-duplicates (remove-if-not #'(lambda (url) (princ ".") (let* ((raw (raw-text url))
                                                                                                 (clean (clean-text raw))
                                                                                                 (vocab (mapcar #'intern (remove-duplicates (split clean #\ ) :test #'equal))))
                                                                                            (and (url-allowed? *crawler-name* url)
                                                                                                 (not (or (equal url best-url)
                                                                                                          (member url queue :test #'equal)
                                                                                                          (member url acc :test #'equal)))
                                                                                                 (> (length vocab) 50) ;; way too short websites aren't too classifiable
                                                                                                 (> (comprehensible-text? raw clean) 0.75) ;; avoiding lots of unknown characters
                                                                                                 (> (comprehensible? vocab) 0.4) ;; avoiding unknown languages
                                                                                                 )))
                                                              (remove-if #'(lambda (link) (or (member (core-domain (find-domain link))
                                                                                                      visited-domains
                                                                                                      :test #'equal)
                                                                                              (equal (core-domain (find-domain link))
                                                                                                     (core-domain (find-domain best-url)))))
                                                                         raw-links))
                                               :test #'equal))))
        (push best-url acc)
        (push (core-domain (find-domain best-url)) visited-domains)
        (append-to-file (concat *crawl-data-folder* "focused-found")
                        (concat (cdr (first (sort (copy-seq queue) #'> :key #'cdr))) " " best-url))
        (push (concat (cdr (first (sort (copy-seq queue) #'> :key #'cdr))) " " best-url) results)
        (setf queue (remove-nth 0 (sort (copy-seq queue) #'> :key #'cdr)))
        (print (concat "found " (write-to-string (length raw-links)) " links, of which " (write-to-string (length links)) " will be checked: "))
        ;; TBD: Make functional
        (dolist (link links)
          (if (not (or (member link acc :test #'equal)
                       (member link queue :key #'car :test #'equal)))
              (let* ((domain-in-queue? (some #'(lambda (elem)
                                                 (equal (core-domain (find-domain (car elem)))
                                                        (core-domain (find-domain link))))
                                             queue))
                     (opponent (if domain-in-queue?
                                   (first (remove-if-not #'(lambda (elem)
                                                             (equal (core-domain (find-domain (car elem)))
                                                                    (core-domain (find-domain link))))
                                                         queue))
                                   (last1 queue)))
                     (opponent-score (cdr opponent))
                     (opponent-url (car opponent))
                     (score (link-score link target)))
                (if (equal (url-text link)
                           "nothingfound")
                    (princ "x")
                    (progn
                      (princ ".")
                      (if (or domain-in-queue?
                              (>= (length queue) queue-size))
                          (setf queue (sort (mapcar #'(lambda (elem)
                                                        (if (and (equal (car elem) opponent-url)
                                                                 (> score opponent-score))
                                                            (cons link score)
                                                            elem))
                                                    queue)
                                            #'> :key #'cdr))
                          (progn (push (cons link score) queue)
                                 (setf queue (sort (copy-seq queue) #'> :key #'cdr)))))))
              (princ "x")))))
    (reverse acc)))

(defun random-pick (lst)
  (nth (random (length lst)) lst))

(defun drunkbot (seed page-count)
  #|
  Perhaps just screw Facebook & Twitter & Instagram completely.
  (safe-fetch-html "http://www.costco.com/robots.txt")
  (SAFE-FETCH-HTML "https://www.mouser.com/robots.txt")
  |#
  (let ((hst (list seed))
        (forbidden (list "https://waitbutwhy.com/feed/")))
    (dotimes (i page-count)
      (let* ((current-url (car hst))
             (raw-links (filter-links (vetted-links current-url)))
             (links (remove-if-not #'(lambda (url) (and (not (equal url current-url))
                                                        (not (member url forbidden :test #'equal))
                                                        (not (member url hst :test #'equal))
                                                        (not (search "twitter" url))
                                                        (not (search "facebook" url))
                                                        (url-allowed? *crawler-name* url)))
                                   raw-links)))
        (if (zerop (print (length links)))
            (progn
              (print ":(") ;not enough if a site sends you into a loop
              (push current-url forbidden)
              (setf hst (remove-if #'(lambda (url) (or (member url forbidden :test #'equal)))
                                   (cdr hst))))
            (progn
              (with-open-file (stream (concat *crawl-data-folder* "wanderbot")
                                      :direction :output :if-exists :append :if-does-not-exist :create)
                (print current-url stream))
              (push (print (random-pick links)) hst)))))
    (reverse hst)))

;; TBD: Place this into correct util
(defun best-key (hashtable pred)
  (best-element (list-keys hashtable) pred #'(lambda (el) (gethash el hashtable))))

(defun best-element (lst pred key)
  (if lst
      (let* ((acc (car lst))
             (max (funcall key acc)))
        (dolist (i lst)
          (if (funcall pred (funcall key i) max)
              (progn
                (setf acc i)
                (setf max (funcall key acc)))))
        (values acc max))))


(defun place (url &optional (class "/"))
  (if (subclasses class)
      (place url (let* ((subclasses (subclasses class))
                        (simple-path-scores (map-to-hash #'cdr
                                                         (list-hashes (scores (remove-duplicates (tokens (url-text url)) :test #'equal)
                                                                              subclasses
                                                                              (map-to-hash #'get-recursive-corpus subclasses)
                                                                              (map-to-hash #'get-word-count subclasses)
                                                                              nil))
                                                         :key-fun #'car)))
                   (best-key simple-path-scores #'>)))
      class))

(defun hop-links (current-url queue acc visited-domains intradomain-cap link-cap)
  (let ((links (shuffle (remove-duplicates (remove-if (if (zerop intradomain-cap)
                                                          #'(lambda (link) (princ ",")
                                                              (or (member (core-domain (find-domain link))
                                                                          visited-domains
                                                                          :test #'equal)
                                                                  (equal (core-domain (find-domain link))
                                                                         (core-domain (find-domain current-url)))))
                                                          #'(lambda (link) (princ ",")
                                                              (or (member link acc :test #'equal)
                                                                  (member link queue :test #'equal)
                                                                  (not (equal (core-domain (find-domain link))
                                                                              (core-domain (find-domain current-url)))))))
                                                      (filter-links (vetted-links current-url)))
                                           :test #'equal))))
    (print (length links))
    (do* ((i 0 (1+ i))
          (url (nth 0 links) (nth i links))
          (result nil (if (progn (princ ":")
                                 (let* ((raw (raw-text url))
                                        (clean (clean-text raw))
                                        (vocab (mapcar #'intern (remove-duplicates (split clean #\ ) :test #'equal))))
                                   (and (url-allowed? *crawler-name* url)
                                        (not (or (equal url current-url)
                                                 (member url queue :test #'equal)
                                                 (member url acc :test #'equal)))
                                        (> (length vocab) 50) ;; way too short websites aren't too classifiable
                                        (> (comprehensible-text? raw clean) 0.75) ;; avoiding lots of unknown characters
                                        (> (comprehensible? vocab) 0.5) ;; avoiding unknown languages
                                        )))
                          (append1 result (nth i links))
                          result)))
         ((or (>= (length result) link-cap)
              (>= (1+ i) (length links)))
          result))))





(defun classbot (seed queue-size page-count)
  (setf *random-state* (make-random-state t))
  (let ((queue (list (cons seed 4)))
        (acc nil)
        (visited-domains nil))
    (dotimes (i page-count)
      (print queue)
      (let* ((choice (nth (random (length queue)) queue))
             (chosen-url (car choice))
             (intradomain-cap (cdr choice))
             (links (shuffle (hop-links chosen-url (mapcar #'car queue) acc visited-domains intradomain-cap (1+ (- queue-size (length queue)))))))
        (push chosen-url acc)
        (push (core-domain (find-domain chosen-url)) visited-domains)
        (setf queue (remove-if #'(lambda (elem) (equal (car elem) chosen-url)) queue))
        (print "!")
        (let* ((folder (concat "../DATA/discovered" (place chosen-url)))
               (path (concat folder "links")))
          
          (ensure-directories-exist path)
          (overwrite-direct-file (concat folder "comment") "auto-generated class")
          (redownload-file chosen-url)
          (overwrite-direct-file path (append1 (fallback (ignore-errors (read-from-file path)) nil) chosen-url)))
        (let ((new-links nil))
          (dolist (link links)
            (princ ".")
            (if (and (< (length queue) queue-size)
                     #|(not (some #'(lambda (url)
                     (equal (core-domain (find-domain url)) ; ;
                     (core-domain (find-domain link)))) ; ;
                     queue))|#
                     (not (equal (url-text link)
                                 "nothingfound")))
                (progn
                  (push link new-links)
                  (push (cons link (if (< intradomain-cap 1)
                                       4
                                       (1- intradomain-cap)))
                        queue))))
          (append-to-file "../DATA/crawlers/classbot-path" (cons chosen-url new-links)))))
    (reverse acc)))

(defun display-path (start)
  ;; wow that is ugly & slow
  (let ((hst (read-from-file "../DATA/crawlers/classbot-path"))
        (queue (list "https://waitbutwhy.com"))
        (reconstructed nil))
    (dolist (elem hst)
      (push queue reconstructed)
      (setf queue (swap queue (car elem) (cdr elem))))
    (ltk:with-ltk ()
      (ltk:withdraw ltk:*tk*)
      (let ((W (window ".")))
        (ltk:on-close W #'(lambda () (ltk:destroy ltk:*tk*)))
        (let ((aliases (make-hash-table :test #'equal))
              (counter 0))
          (dotimes (i (- (length reconstructed) start))
            (dotimes (j (length (nth i (reverse reconstructed))))
              (let* ((x (+ start i))
                     (y j)
                     (link (nth y (nth x (reverse reconstructed))))
                     (b (if link
                            (button x y W
                                    (write-to-string (fallback (gethash link aliases)
                                                               (setf (gethash link aliases) (incf counter))))
                                    #'(lambda () (info-box link "baf"))))))
                (if link
                    (ltk:configure b :foreground "#ffffff"))
                (if link
                    (ltk:configure b :background ; "#000000"
                                   (if (member (nth y (nth x (reverse reconstructed)))
                                               (nth (min (length reconstructed) (1+ x)) (reverse reconstructed))
                                               :test #'equal)
                                       (if (member (nth y (nth x (reverse reconstructed)))
                                                   (nth (max 0 (1- x)) (reverse reconstructed))
                                                   :test #'equal)
                                           "#000000"
                                           "#008800")
                                       (if (member (nth y (nth x (reverse reconstructed)))
                                                   (nth (max 0 (1- x)) (reverse reconstructed))
                                                   :test #'equal)
                                           "#880088"
                                           "#444444"))))))))))))

(defun swap (lst elem new-seq)
  (let ((acc nil))
    (dotimes (i (length lst))
      (if (equal (nth i lst) elem)
          (setf acc (append (subseq lst 0 i)
                            new-seq
                            (subseq lst (min (length lst)
                                             (1+ i)))))))
    acc))
