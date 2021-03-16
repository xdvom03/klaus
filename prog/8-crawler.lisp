(defun reset-file-folder (name)
  ;; TEMP for crawler
  (setf *files-folder* (concat "../DATA/" name "/"))
  (setf *html-folder* (concat *files-folder* "html/"))
  (setf *text-folder* (concat *files-folder* "text/"))
  (setf *core-text-folder* (concat *files-folder* "core/"))
  (setf *aliases-file* (concat *files-folder* "file-aliases")))

(defun swap (lst elem new-seq)
  (let ((acc nil))
    (dotimes (i (length lst))
      (if (equal (nth i lst) elem)
          (setf acc (append (subseq lst 0 i)
                            new-seq
                            (subseq lst (min (length lst)
                                             (1+ i)))))))
    acc))

#|
TBD: Properly cut off trailing section hashtags and other useless url fluff
TBD: Properly cache url scores, profile
Not using a robots.txt library because is has no license.
TBD: Cache robots.txt (or a sensible format thereof)

Crawl 40 from:

"https://www.waitbutwhy.com"
"https://en.wikipedia.org" (got stuck)
"https://www.lifehack.org/891214/happiness-book"
"https://www.jscc.edu/academics/programs/writing-center/writing-resources/five-paragraph-essay.html"
|#

(defun prob (vocab class)
  (let* ((sibling-classes (subclasses (parent-class class)))
         (path-scores (scores vocab
                              sibling-classes
                              (map-to-hash #'get-recursive-corpus sibling-classes)
                              (map-to-hash #'get-word-count sibling-classes)
                              nil)))
    (gethash class path-scores)))

(defun url-score (url target-class)
  (vocab-score (remove-duplicates (wordlist (url-text url))) target-class))

(defun vocab-score (vocab target-class)
  (let ((path "/")
        (acc 0)
        (prob 1))
    (dolist (frag (cl-strings:split (subseq target-class 1) #\/))
      (if (not (equal frag ""))
          (progn
            (setf path (concat path frag "/"))
            (setf prob (* prob (prob vocab path)))
            (incf acc prob))))
    prob))

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

(defun place-known (url &optional (class "/"))
  (place-vocab (remove-duplicates (wordlist (read-text url))) class))

(defun place (url &optional (class "/"))
  (place-vocab (remove-duplicates (wordlist (url-text url))) class))

(defun place-vocab (vocab &optional (class "/"))
  (let ((subclasses (subclasses class)))
    (if (subclasses class)
        (let* ((scores (scores vocab
                               subclasses
                               (map-to-hash #'get-recursive-corpus subclasses)
                               (map-to-hash #'get-word-count subclasses)
                               nil))
               (best-path (best-key scores #'>)))
          (place-vocab vocab best-path))
        class)))

(defun place-discovered (url)
  (redownload-file url)
  (let* ((folder (concat *discovered-folder* (place-vocab (remove-duplicates (wordlist (read-text url))))))
         (path (concat folder "urls")))
    (ensure-directories-exist path)
    (overwrite-file (concat folder "comment") "auto-generated class")
    (redownload-file url)
    (overwrite-file path (append1 (fallback (ignore-errors (read-from-file path)) nil) url))))

(defun zoombot-valuation (vocab target)
  (princ ".")
  (let* ((actual-place (place-vocab vocab))
         (overlap-length (overlap-length (cl-strings:split actual-place #\/)
                                         (cl-strings:split target #\/)))
         (final-folder (concat (cl-strings:join (subseq (cl-strings:split target #\/)
                                                        0
                                                        (min (length (cl-strings:split target #\/))
                                                             (1+ overlap-length)))
                                     :separator "/")
                               "/")))
    (+ overlap-length (vocab-score vocab final-folder))))

(defun allowed-url? (url)
  (if (url-allowed? *crawler-name* url)
      (handler-case (progn
                      (redownload-file url)
                      (let* (;(raw (read-raw url))
                             (text (read-text url))
                             (vocab (wordlist text)))
                        (if (and (> (length vocab) *min-word-count*) ;; way too short websites aren't classifiable
                                 ;; let's just assume that these sites might not contain invalid chars anyway, right? (> (comprehensible-text? raw text) *min-character-comprehensibility*) ;; avoiding lots of unknown characters
                                 (> (comprehensible? vocab) *min-word-comprehensibility*)) ;; avoiding unknown languages
                            (princ "o")
                            (progn
                              (princ ".")
                              nil))))
        ;; Any error will cause the system to refuse the link (typically because the site is unreachable or contains invalid content)
        (error (err-text)
          (declare (ignore err-text))
          nil))
      (princ "X")))

(defun zoombot-url-value (url target)
  (redownload-file url)
  (zoombot-valuation (remove-duplicates (wordlist (read-text url))) target)) ; tady mazeme duplikaty. holt to bude chtit byt chytrejsi

(defun zoombot (seed steps domain-steps target)
  (setf *random-state* (make-random-state t))
  
  (let* ((acc nil)
         (visited-domains (map-to-hash #'(lambda (pair) (declare (ignore pair)) t)
                                       (list-keys (url-aliases))
                                       :key-fun #'find-domain))
         (current-url seed)
         (current-url-score (zoombot-url-value current-url target))
         (domain-limit domain-steps)
         (blacklist (make-hash-table :test #'equal))
         (score 0))
    
    (dotimes (i steps)
      (terpri)
      (terpri)
      (princ (concat domain-limit " " i " " (length acc) " " (- (length acc) (length (list-keys blacklist))) " " ))
      (princ current-url)
      (terpri)

      (redownload-file current-url)

      ;; save url
      (if (not (find current-url acc :test #'equal))
          (progn
            (push current-url acc)
            (setf (gethash (find-domain current-url) visited-domains) t)
            (discover current-url)
            (append-to-file "../DATA/scores" (concat (my-round current-url-score) " " current-url))))

      (print "file saved")

      ;; find linked urls
      (let* ((domain (find-domain current-url))
             (raw-urls (filter-urls (downloaded-vetted-links current-url)))
             (urls (progn (print (concat "Raw: " (length raw-urls)))
                          (remove-if #'(lambda (url) (or (find url acc :test #'equal)
                                                         (equal url current-url)
                                                         (equal (quri:uri-scheme (quri:uri url)) "mailto")))
                                     raw-urls)))
             (domain-urls (progn (print (concat "Step 1: " (length urls)))
                                 (shuffle (if (zerop domain-limit)
                                              (remove-if #'(lambda (url) (gethash (find-domain url) visited-domains))
                                                         urls)
                                              (remove-if-not #'(lambda (url) (equal (find-domain url)
                                                                                    domain))
                                                             urls)))))

             (allowed-urls (progn (print (concat "Valid domain: " (length domain-urls)))
                                  (remove-if-not #'allowed-url? (subseq domain-urls 0 (min (length domain-urls) 25))))))

        (print (concat "Allowed: " (length allowed-urls)))
        
        (let ((scores (map-to-hash #'(lambda (url) (zoombot-url-value url target))
                                   allowed-urls)))
          (multiple-value-bind (chosen-url chosen-score)
              (best-element allowed-urls #'> #'(lambda (url) (gethash url scores)))
            (setf current-url-score chosen-score)
            (if chosen-url
                (progn
                  (setf domain-limit (mod (1- domain-limit)
                                          domain-steps))
                  (setf current-url chosen-url)
                  (incf score))
                (progn
                  (print "retreat!")
                  (terpri)
                  (terpri)
                  (decf score)
                  (setf (gethash current-url blacklist) t)
                  (setf current-url (first (remove-if #'(lambda (url) (gethash url blacklist))
                                                      acc)))))))))
    (print (concat "score: " score "/" steps))
    (print (reverse acc))
    (reverse (remove-if #'(lambda (url) (gethash url blacklist))
                        acc))))

















(defun discover (url)
  (let* ((folder (concat *discovered-folder* (place-vocab (remove-duplicates (wordlist (read-text url))))))
         (path (concat folder "urls")))
    (ensure-directories-exist path)
    (overwrite-file (concat folder "comment") "auto-generated class")
    (overwrite-file path (append1 (ignore-errors (read-from-file path)) url))))

(defun rediscover ()
  (let ((counter 0)
        (urls (read-from-file "../DATA/viewed-once")))
    (dolist (url urls)
      (print (concat (incf counter) "/" (length urls)))
      (discover (print url)))))

(defun sorted-bot-urls ()
  (let ((urls (read-from-file "../DATA/bot-path")))
    (sort (copy-seq urls) #'< :key #'(lambda (url) (length (wordlist (read-text url)))))))




;; (apply-to-all-classes #'(lambda (path) (if (not (directory (concat (full-path path) "urls"))) (overwrite-file path "urls" nil))))
;; (apply-to-all-classes #'(lambda (path) (if (not (directory (concat (full-path path) "comment"))) (overwrite-file path "comment" "placeholder"))))
;; TBD: Call error if file has no alias!





(defun raw-url (url)
  (let* ((step1 (subseq url (+ 2 (search "//" url))))
         (step2 (reverse (subseq (reverse step1)
                                 (position-if #'(lambda (character) (not (equal character #\/)))
                                              (reverse step1))))))
    (if (equal (subseq step2 0 4) "www.")
        (subseq step2 4)
        step2)))

(defun nonredundant-bot-path ()
  (let ((lst (read-from-file "../DATA/bot-path")))
    (print (length lst))
    (remove-duplicates lst :test #'(lambda (url1 url2) (if (zerop (random 1000000)) (princ ".")) (equal (raw-url url1) (raw-url url2))))))

(defun nonredundant-bot-urls ()
  (let ((lst (read-from-file "../DATA/bot-viewed")))
    (print (length lst))
    (remove-duplicates lst :test #'(lambda (url1 url2) (if (zerop (random 1000000)) (princ ".")) (equal (raw-url url1) (raw-url url2))))))

(defun backbot (seed steps domain-steps)
  (setf *random-state* (make-random-state t))
  
  (let ((acc nil)
        (visited-domains (map-to-hash #'(lambda (pair) (declare (ignore pair)) t)
                                      (list-keys (url-aliases))
                                      :key-fun #'find-domain))
        (current-url seed)
        (domain-limit domain-steps)
        (blacklist (make-hash-table :test #'equal))
        (score 0))
    
    (dotimes (i steps)
      (princ (concat domain-limit " " i " " (length acc) " " (- (length acc) (length (list-keys blacklist))) " " ))
      (princ current-url)
      (terpri)

      (redownload-file current-url)

      ;; save url
      (if (not (find current-url acc :test #'equal))
          (progn
            (push current-url acc)
            (setf (gethash (find-domain current-url) visited-domains) t)
            (discover current-url)))

      ;; find linked urls
      (let* ((domain (find-domain current-url))
             (urls (remove-if #'(lambda (url) (or (find url acc :test #'equal)
                                                  (equal url current-url)
                                                  (equal (quri:uri-scheme (quri:uri url)) "mailto")))
                              (filter-urls (downloaded-vetted-links current-url))))
             (domain-urls (shuffle (if (zerop domain-limit)
                                       (remove-if #'(lambda (url) (gethash (find-domain url) visited-domains))
                                                  urls)
                                       (remove-if-not #'(lambda (url) (equal (find-domain url)
                                                                             domain))
                                                      urls))))
             
             (chosen-url (do ((counter 0 (1+ counter)))
                             ((or (>= counter (length domain-urls))
                                  (allowed-url? (nth counter domain-urls)))
                              (nth counter domain-urls)))))
        (if chosen-url
            (progn
              (setf domain-limit (mod (1- domain-limit)
                                      domain-steps))
              (setf current-url chosen-url)
              (incf score))
            (progn
              (print "retreat!")
              (terpri)
              (terpri)
              (decf score)
              (setf (gethash current-url blacklist) t)
              (setf current-url (first (remove-if #'(lambda (url) (gethash url blacklist))
                                                  acc)))))))
    (print (concat "score: " score "/" steps))
    (print (reverse acc))
    (reverse (remove-if #'(lambda (url) (gethash url blacklist))
                        acc))))

;; BUG: Yeeting into a place with no internal links, then backtracking, then getting a bunch of new internal links. Sounds like a good way to get stuck in a loop.
