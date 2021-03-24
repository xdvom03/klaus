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

(defun targeted-place (url target &optional (class "/"))
  (targeted-place-vocab (remove-duplicates (wordlist (url-text url))) target class))

(defun place (url &optional (class "/"))
  (place-vocab (remove-duplicates (wordlist (url-text url))) class))

(defun targeted-place-vocab (vocab target &optional (class "/"))
  (let ((subclasses (subclasses class)))
    (if (subclasses class)
        (let* ((scores (scores vocab
                               subclasses
                               (map-to-hash #'get-recursive-corpus subclasses)
                               (map-to-hash #'get-word-count subclasses)
                               nil))
               (best-path (best-key scores #'>)))
          (if (equal 0 (search best-path target))
              (place-vocab vocab best-path)
              class))
        class)))

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

(defun zoombot-url-value (url target)
  (princ ".")
  (let* ((actual-place (discover url))
         (overlap-length (overlap-length (cl-strings:split actual-place #\/)
                                         (cl-strings:split target #\/)))
         (final-folder (concat (cl-strings:join (subseq (cl-strings:split target #\/)
                                                        0
                                                        (min (length (cl-strings:split target #\/))
                                                             (1+ overlap-length)))
                                                :separator "/")
                               "/")))
    (+ (1- overlap-length) (vocab-score (remove-duplicates (wordlist (read-text url))) final-folder))))

(defun allowed-url? (url)
  (if (url-allowed? *crawler-name* url)
      (handler-case (progn
                      (redownload-url url)
                      (let* ((text (read-text url))
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
          (princ "E")
          nil))
      (princ "X")))

(defun valid-scheme? (url)
  (ignore-errors (or (equal (quri:uri-scheme (quri:uri url)) "http")
                     (equal (quri:uri-scheme (quri:uri url)) "https"))))

(defun chosen-links (starting-url visited-urls visited-domains same-domain?)
  (redownload starting-url)
  (let* ((domain (find-domain starting-url))
         (raw-urls (filter-urls (downloaded-vetted-links starting-url)))
         (urls (progn (print (concat "Raw: " (length raw-urls)))
                      (remove-if #'(lambda (url) (or (find url visited-urls :test #'equivalent-urls)
                                                     (not (valid-scheme? url))))
                                 raw-urls)))
         (domain-urls (progn (print (concat "Step 1: " (length urls)))
                             (shuffle (if same-domain?
                                          (remove-if-not #'(lambda (url)
                                                             (princ "!")
                                                             (equal (find-domain url)
                                                                    domain))
                                                         urls)
                                          (remove-if #'(lambda (url) (gethash (find-domain url) visited-domains))
                                                     urls)))))
         (allowed-urls (progn (print (concat "Valid domain: " (length domain-urls)))
                              (remove-if-not #'allowed-url? (subseq domain-urls 0 (min (length domain-urls) *link-cap*)))))
         
         ;; TEMP: Redundakitties galore!
         (origin-urls (progn (print (concat "Allowed: " (length allowed-urls)))
                             (remove-if #'(lambda (origin-url)
                                            (or (find origin-url visited-urls :test #'equivalent-urls)
                                                (not (valid-scheme? origin-url))
                                                (if same-domain?
                                                    (not (equal (find-domain origin-url) domain))
                                                    (gethash (find-domain origin-url) visited-domains))
                                                (not (allowed-url? origin-url))))
                                        (remove-duplicates (mapcar #'read-origin
                                                                   allowed-urls)
                                                           :test #'equivalent-urls)))))
    (print (concat "Valid origin: " (length origin-urls)))
    origin-urls))

(defun domain-links (seed target count)
  ;; visits links in succession (different from interdomain, may change) to get wider site coverage
  ;; if it finds no valid links, it keeps trying again (maybe getting other links)
  (let ((scores (ht))
        (visited (list seed))
        (current-url seed))
    (dotimes (i count)
      (setf (gethash current-url scores)
            (zoombot-url-value current-url target))
      (append-to-file "../DATA/scores" (concat (my-round (gethash current-url scores)) " " current-url))
      (let* ((links (chosen-links current-url visited (ht) t))
             (link-scores (map-to-hash #'(lambda (url) (zoombot-url-value url target))
                                       links)))
        (if links
            (multiple-value-bind (url score)
                (best-element (reverse links) #'> #'(lambda (url) (gethash url link-scores)))
              (setf (gethash url scores) score)
              (setf current-url url)
              (push url visited)))))
    (values visited scores)))

(defun zoombot (seed domains per-domain target)
  (setf *random-state* (make-random-state t))
  (refresh-comprehensible-corp)
  (let* ((visited-urls (list seed))
         (followed-urls nil)
         (url-scores (ht))
         (visited-domains (ht)))
    (setf (gethash seed url-scores) (zoombot-url-value seed target))

    (dotimes (i domains)
      (let* ((starting-url (best-element (set-difference visited-urls followed-urls) #'> #'(lambda (url) (gethash url url-scores))))
             (links (chosen-links starting-url visited-urls visited-domains nil))
             (link-scores (map-to-hash #'(lambda (url) (zoombot-url-value url target))
                                       links)))
        (print "starting from: ")
        (print starting-url)
        ;(print "links:")
                                        ;(print links)
        ;; crashes if no interdomain links are found
        (push starting-url followed-urls)
        (if links
            (multiple-value-bind (chosen-url chosen-score)
                (best-element links #'> #'(lambda (url) (gethash url link-scores)))
              (print (concat "Best URL: " chosen-url " score: " chosen-score))
              (setf (gethash (find-domain chosen-url) visited-domains) t)
              (multiple-value-bind (domain-links domain-scores)
                  (domain-links chosen-url target per-domain)
                (dolist (link domain-links)
                  (push link visited-urls)
                  (setf (gethash link url-scores)
                        (gethash link domain-scores)))))
            (print "No intradomain links found."))))))

















(defun discover (url)
  ;; TBD: Is taking each word once the right thing here?
  ;; roughly 50% of time is spent classing, 25% reading files (to see if we should write). TBD: Work in memory.
  (redownload-url url)
  (let* ((class (place-vocab (remove-duplicates (wordlist (read-text url)))))
         (folder (concat *discovered-folder* class))
         (path (concat folder "urls")))
    (ensure-directories-exist path)
    (if (not (member url (discovered-urls class) :test #'equal))
        (overwrite-file path (append1 (ignore-errors (read-from-file path)) url)))
    class))

(defun sorted-bot-urls ()
  (let ((urls (read-from-file "../DATA/bot-path")))
    (sort (copy-seq urls) #'< :key #'(lambda (url) (length (wordlist (read-text url)))))))




;; (apply-to-all-classes #'(lambda (path) (if (not (directory (concat (full-path path) "urls"))) (overwrite-file path "urls" nil))))
;; (apply-to-all-classes #'(lambda (path) (if (not (directory (concat (full-path path) "comment"))) (overwrite-file path "comment" "placeholder"))))
;; TBD: Call error if file has no alias!
;; TBD: Let us see URL places without building an entire corpus (through a minimal view)



(defun equivalent-urls (url1 url2)
  (equal (raw-url url1)
         (raw-url url2)))

(defun raw-url (url) ;; TBD: QURI
  (if (search "//" url)
      (let* ((step1 (subseq url (+ 2 (search "//" url))))
             (step2 (reverse (subseq (reverse step1)
                                     (position-if #'(lambda (character) (not (equal character #\/)))
                                                  (reverse step1))))))
        (if (equal 0 (search "www." step2))
            (subseq step2 4)
            step2))
      url))

(defun place-discovered (url)
  (redownload-url url)
  (let* ((folder (concat *discovered-folder* (place-vocab (remove-duplicates (wordlist (read-text url))))))
         (path (concat folder "urls")))
    (ensure-directories-exist path)
    (overwrite-file (concat folder "comment") "auto-generated class")
    (overwrite-file path (append1 (fallback (ignore-errors (read-from-file path)) nil) url))))
