(defun reset-file-folder (name)
  ;; TEMP for crawler
  (setf *files-folder* (concat "../DATA/" name "/"))
  (setf *html-folder* (concat *files-folder* "html/"))
  (setf *text-folder* (concat *files-folder* "text/"))
  (setf *core-text-folder* (concat *files-folder* "core/"))
  (setf *aliases-file* (concat *files-folder* "file-aliases/")))

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

(defun random-pick (queue acc)
  (if queue
      (let ((choice (nth (random (length queue)) queue)))
        (values (car choice)
                (cdr choice)))
      ;; If we ran out of queue, do a Hail Mary and start over from a random point visited.
      (progn
        (print "Hailmary")
        (values (nth (random (length acc)) acc)
                0))))

(defun allowed-url? (url)
  (if (url-allowed? *crawler-name* url)
      (progn
        (redownload-file url)
        (let* ((raw (extract-raw-text (read-html url)))
               (text (clean-text raw))
               (vocab (remove-duplicates (wordlist text))))
          (and (not (equal text "nothingfound"))
               (> (length vocab) 50) ;; way too short websites aren't classifiable
               (> (comprehensible-text? raw text) 0.75) ;; avoiding lots of unknown characters
               (> (comprehensible? vocab) 0.5) ;; avoiding unknown languages
               )))))

(defun backbot (seed steps domain-steps)
  (setf *random-state* (make-random-state t))
  
  (let ((acc nil)
        (visited-domains (map-to-hash #'(lambda (link) (declare (ignore link)) t)
                                      (aliases)
                                      :key-fun (compose #'core-domain #'find-domain #'car)))
        (current-url seed)
        (domain-limit domain-steps)
        (blacklist (make-hash-table :test #'equal))
        (score 0))
    
    (dotimes (i steps)
      (princ (concat domain-limit " " i " " (length acc) " " (- (length acc) (length (list-keys blacklist))) " " ))
      (princ current-url)
      (terpri)

      ;; save url
      (if (not (find current-url acc :test #'equal))
          (progn
            (push current-url acc)
            (setf (gethash (core-domain (find-domain current-url)) visited-domains) t)
            (redownload-file current-url)
            (let* ((folder (concat *discovered-folder* (place-vocab (remove-duplicates (wordlist (read-text current-url))))))
                   (path (concat folder "links")))
              (ensure-directories-exist path)
              (overwrite-direct-file (concat folder "comment") "auto-generated class")
              (overwrite-direct-file path (append1 (ignore-errors (read-from-file path)) current-url)))))

      ;; find links
      (let* ((domain (core-domain (find-domain current-url)))
             (links (remove-if #'(lambda (link) (or (find link acc :test #'equal)
                                                    (equal link current-url)
                                                    (equal (subseq link 0 (min 7 (length link))) "mailto:")))
                               (filter-links (vetted-links current-url))))
             (domain-links (shuffle (if (zerop domain-limit)
                                        (remove-if #'(lambda (link) (gethash (core-domain (find-domain link)) visited-domains))
                                                   links)
                                        (remove-if-not #'(lambda (link) (equal (core-domain (find-domain link))
                                                                               domain))
                                                       links))))
             
             (chosen-link (do ((counter 0 (1+ counter)))
                              ((or (>= counter (length domain-links))
                                   (allowed-url? (nth counter domain-links)))
                               (nth counter domain-links)))))
        (if chosen-link
            (progn
              (setf domain-limit (mod (1- domain-limit)
                                      domain-steps))
              (setf current-url chosen-link)
              (incf score))
            (if (zerop domain-limit)
                (progn
                  (print "retreat!")
                  (terpri)
                  (terpri)
                  (decf score)
                  (setf (gethash current-url blacklist) t)
                  (setf current-url (first (remove-if #'(lambda (url) (gethash url blacklist))
                                                      acc))))
                (progn
                  ;; if we cannot link within the domain, might as well try going elsewhere
                  (setf domain-limit 0)
                  (print "HAIL MARY")
                  (terpri)
                  (terpri))))))
    (print (concat "score: " score "/" steps))
    (print (reverse acc))
    (reverse (remove-if #'(lambda (url) (gethash url blacklist))
                        acc))))






;; (apply-to-all-classes #'(lambda (path) (if (not (directory (concat (full-path path) "links"))) (overwrite-file path "links" nil))))
