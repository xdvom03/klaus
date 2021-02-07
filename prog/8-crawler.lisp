(defun swap (lst elem new-seq)
  (let ((acc nil))
    (dotimes (i (length lst))
      (if (equal (nth i lst) elem)
          (setf acc (append (subseq lst 0 i)
                            new-seq
                            (subseq lst (min (length lst)
                                             (1+ i)))))))
    acc))

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

#|
TBD: Properly cut off trailing section hashtags and other useless link fluff
TBD: Properly cache link scores, profile
Not using a robots.txt library because is has no license.
TBD: Cache robots.txt (or a sensible format thereof)
BUG: Can get stuck on: "Open too many files" or scoring "http://lightspeed.sourceforge.net/". Make it fail gracefully in these cases, releasing the queue AND acc with each major visited file (not just scored, but actually taken).

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
  (vocab-score (remove-duplicates (wordlist (url-text url))) target-class))

(defun vocab-score (vocab target-class)
  (let ((path "/")
        (acc 0)
        (prob 1))
    (dolist (frag (split (subseq target-class 1) #\/))
      (if (not (equal frag ""))
          (progn
            (setf path (concat path frag "/"))
            (setf prob (* prob (prob vocab path)))
            (incf acc prob))))
    prob))

(defun core-domain (domain)
  ;; Have to deal with trailing dot
  (second (remove-if #'(lambda (part) (equal part ""))
                     (reverse (split (last1 (split domain #\/)) #\.)))))

(defun append-to-file (path txt)
  (with-open-file (stream path :direction :output :if-exists :append :if-does-not-exist :create)
    (print txt stream)))

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
  (place-vocab (remove-duplicates (wordlist (url-text url))) class))

(defun place-vocab (vocab &optional (class "/"))
  (if (subclasses class)
      (place-vocab vocab (let* ((subclasses (subclasses class))
                                (simple-path-scores (map-to-hash #'cdr
                                                                 (list-hashes (scores vocab
                                                                                      subclasses
                                                                                      (map-to-hash #'get-recursive-corpus subclasses)
                                                                                      (map-to-hash #'get-word-count subclasses)
                                                                                      nil))
                                                                 :key-fun #'car)))
                           (best-key simple-path-scores #'>)))
      class))

(defun place-discovered (link)
  (redownload-file link)
  (let* ((folder (concat *discovered-folder* (place-vocab (remove-duplicates (wordlist (read-text link))))))
         (path (concat folder "links")))
    (ensure-directories-exist path)
    (overwrite-direct-file (concat folder "comment") "auto-generated class")
    (redownload-file link)
    (overwrite-direct-file path (append1 (fallback (ignore-errors (read-from-file path)) nil) link))))

(defun pick-the-best (queue)
  (let ((item (best-element queue #'> #'third)))
    (values (first item)
            (second item)
            (third item))))

(defun pursued-link-vocab (links domain visited-domains visited-links change-domain?)
  (terpri)
  (let ((acc (make-hash-table :test #'equal))
        (novel-links (remove-if #'(lambda (novel-link) (member novel-link visited-links :test #'equal)) links)))
    (dolist (link (if change-domain?
                      (remove-if #'(lambda (link) (member (core-domain (find-domain link))
                                                          visited-domains :test #'equal))
                                     novel-links)
                      (remove-if-not #'(lambda (link) (equal (core-domain (find-domain link))
                                                             domain))
                                     novel-links)))
      (if (url-allowed? *crawler-name* link)
          (progn
            (redownload-file link) ;;; Redundant?
            (let* ((raw (extract-raw-text (read-html link)))
                   (text (clean-text raw))
                   (vocab (remove-duplicates (wordlist text))))
              (if (and (url-allowed? *crawler-name* link)
                       (not (equal text "nothingfound"))
                       (> (length vocab) 50) ;; way too short websites aren't classifiable
                       (> (comprehensible-text? raw text) 0.75) ;; avoiding lots of unknown characters
                       (> (comprehensible? vocab) 0.5) ;; avoiding unknown languages
                       )
                  (progn
                    (setf (gethash link acc) vocab)
                    (princ ",")))))))
    (terpri)
    acc))

(defun queuebot (seed queue-size domain-cap page-count choice-fun value-fun link-filtering-fun)
  ;; General bot to be given smart evaluation functions
  ;; Places everything it finds
  ;; value-fun operates on vocab!!!

  (if (directory "../DATA/files/file-aliases")
      (error "Move existing files elsewhere! This will spam them!")
      (overwrite-direct-file "../DATA/files/file-aliases" ()))
  
  (setf *random-state* (make-random-state t))
  
  (let ((queue (list (list seed domain-cap 0)))
        (acc nil)
        (visited-domains nil))
    (dotimes (i page-count)
      (print "queue")
      (print queue)
      (multiple-value-bind (chosen-url intradomain-cap score) (funcall choice-fun queue)

        ;; saving the found stuff
        (push chosen-url acc)
        (push (core-domain (find-domain chosen-url)) visited-domains)
        (setf queue (remove-if #'(lambda (elem) (equal (car elem) chosen-url)) queue))
        (let* ((folder (concat *discovered-folder* (place chosen-url)))
               (path (concat folder "links")))
          
          (ensure-directories-exist path)
          (overwrite-direct-file (concat folder "comment") "auto-generated class")
          (overwrite-direct-file path (append1 (fallback (ignore-errors (read-from-file path))
                                                         nil)
                                               chosen-url))
          (append-to-file (concat *crawl-data-folder* "queuebot-scores")
                          (concat chosen-url " " score)))

        ;; link stuff
        ;; filtering fun needs arguments: links domain visited-domains visited-links change-domain?
        (let* ((vocab (funcall link-filtering-fun
                               (filter-links (vetted-links chosen-url))
                               (core-domain (find-domain chosen-url))
                               visited-domains
                               (append (mapcar #'car queue) acc)
                               (< intradomain-cap 1)))
               (link-scores (map-to-hash #'(lambda (link) (funcall value-fun (gethash link vocab)))
                                         (list-keys vocab)))
               (used-links (pick-n-best link-scores (- queue-size (length queue)) #'>)))
          
          (dolist (link (list-keys used-links))
            (push (list link
                        (if (< intradomain-cap 1)
                            domain-cap
                            (1- intradomain-cap))
                        (gethash link used-links))
                  queue)))))
    (reverse acc)))

(defun pick-n-best (hashtable n pred)
  (let ((lst (sort (mapcar #'(lambda (key) (cons key (gethash key hashtable)))
                           (list-keys hashtable))
                   pred :key #'cdr)))
    (map-to-hash #'cdr
                 (subseq lst 0 (min n (length lst)))
                 :key-fun #'car)))

(defun zoombot-valuation (vocab target)
  (princ ".")
  (let* ((actual-place (place-vocab vocab))
         (overlap-length (overlap-length (split actual-place #\/)
                                         (split target #\/)))
         (final-folder (concat (join (subseq (split target #\/)
                                             0
                                             (min (length (split target #\/))
                                                  (1+ overlap-length)))
                                     "/")
                               "/")))
    (+ overlap-length (vocab-score vocab final-folder))))

(defun zoombot (seed target queue-size domain-cap page-count)
  (queuebot seed queue-size domain-cap page-count #'pick-the-best #'(lambda (vocab) (zoombot-valuation vocab target)) #'pursued-link-vocab))

(defun classbot (seed queue-size domain-cap page-count)
  #|(if (directory "../DATA/files/file-aliases")
      (error "Move existing files elsewhere! This will spam them!")
      (overwrite-direct-file "../DATA/files/file-aliases" ()))|#
  
  (setf *random-state* (make-random-state t))
  
  (let ((queue (list (cons seed domain-cap)))
        (acc nil)
        (visited-domains nil)
        ;(html-db (make-hash-table :test #'equal))
        ;(text-db (make-hash-table :test #'equal))
        )
    (dotimes (i page-count)
      (print "queue")
      (print queue)
      (print i)
      
      (multiple-value-bind (chosen-url intradomain-cap) (random-pick queue)

        ;; saving the found stuff
        (push chosen-url acc)
        (push (core-domain (find-domain chosen-url)) visited-domains)
        (setf queue (remove-if #'(lambda (elem) (equal (car elem) chosen-url)) queue))

        (redownload-file chosen-url) ; just in case


        (let* ((folder (concat *discovered-folder* (place-vocab (remove-duplicates (wordlist (read-text chosen-url) ; (gethash chosen-url text-db)
                                                                                             )))))
               (path (concat folder "links")))
          (ensure-directories-exist path)
          (overwrite-direct-file (concat folder "comment") "auto-generated class")
          (overwrite-direct-file path (append1 (fallback (ignore-errors (read-from-file path)) nil) chosen-url)))


        

        ;; link stuff        
        (let* ((domain (core-domain (find-domain chosen-url)))
               (links (remove-if #'(lambda (link) (or (member link (append acc queue) :test #'equal)
                                                      (equal (subseq link 0 (min 7 (length link))) "mailto:")))
                                 (filter-links (vetted-links chosen-url))))
               (pursued-links (shuffle (if (zerop intradomain-cap)
                                           (remove-if #'(lambda (link) (member (core-domain (find-domain link))
                                                                               visited-domains :test #'equal))
                                                      links)
                                           (remove-if-not #'(lambda (link) (equal (core-domain (find-domain link))
                                                                                  domain))
                                                          links))))
               (chosen-links (let ((n (- queue-size (length queue))))
                               (do ((acc nil)
                                    (counter 0 (1+ counter)))
                                   ((or (= (length acc) n)
                                        (>= counter (length pursued-links)))
                                    acc)
                                 (let* ((url (nth counter pursued-links)))
                                   (if (url-allowed? *crawler-name* url)
                                       (progn
                                         (redownload-file url)
                                         #|(fallback (gethash url html-db)
                                                   (setf (gethash url html-db) (safe-fetch-html url)))
                                         (fallback (gethash url text-db)
                                                   (setf (gethash url text-db) (extract-text (gethash url html-db))))|#
                                         (let* ((raw (extract-raw-text (read-html url) ; (gethash url html-db)
                                                                       ))
                                                (text (clean-text raw))
                                                (vocab (remove-duplicates (wordlist text))))
                                           (if (and (not (equal text "nothingfound"))
                                                    (> (length vocab) 50) ;; way too short websites aren't classifiable
                                                    (> (comprehensible-text? raw text) 0.75) ;; avoiding lots of unknown characters
                                                    (> (comprehensible? vocab) 0.5) ;; avoiding unknown languages
                                                    )
                                               (push url acc))))))))))
          
          (dolist (link chosen-links)
            (print link)
            (push (cons link
                        (if (< intradomain-cap 1)
                            domain-cap
                            (1- intradomain-cap)))
                  queue))
          (append-to-file "../DATA/crawlers/classbot-path" (cons chosen-url chosen-links)))))
    (reverse acc)))

(defun random-pick (queue)
  (if queue
      (let ((choice (nth (random (length queue)) queue)))
        (values (car choice)
                (cdr choice)))
      ;; If we ran out of queue, do a Hail Mary and start over from a random point visited.
      (let ((visited (mapcar #'car (aliases))))
        (print "Hailmary")
        (values (nth (random (length visited)) visited)
                0))))















(defun reset-file-folder (name)
  ;; TEMP for crawler
  (setf *files-folder* (concat "../DATA/" name "/"))
  (setf *html-folder* (concat *files-folder* "html/"))
  (setf *text-folder* (concat *files-folder* "text/"))
  (setf *core-text-folder* (concat *files-folder* "core/"))
  (setf *aliases-file* (concat *files-folder* "file-aliases/")))
