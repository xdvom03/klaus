#|
TBD: Properly cut off trailing section hashtags and other useless link fluff
TBD: Properly cache link scores, profile
Not using a robots.txt library because is has no license.
BUG: Unsaved comment message
TBD: Cache robots.txt (or a sensible format thereof)
BUG: Can get stuck on: "Open too many files" or scoring "http://lightspeed.sourceforge.net/". Make it fail gracefully in these cases, releasing the queue AND acc with each major visited file (not just scored, but actually taken).

Sometimes gets stuck on a random URL for seemingly no reason. TBD: Do time requests, screw it after half a minute.

Crawl 40 from:

"https://www.waitbutwhy.com"
"https://en.wikipedia.org" (got stuck)
"https://www.lifehack.org/891214/happiness-book"
"https://www.jscc.edu/academics/programs/writing-center/writing-resources/five-paragraph-essay.html"
|#

(defun compose (&rest functions)
  (if (one-elem? functions)
      (car functions)
      (lambda (param)
        (funcall (car functions)
                 (funcall (apply #'compose (cdr functions))
                          param)))))

(defun prob (vocab folder)
  (let ((simple-path-scores (map-to-hash #'cdr
                                         (list-hashes (scores vocab
                                                              (subfolders (parent-folder folder))
                                                              (map-to-hash #'get-recursive-corpus (subfolders (parent-folder folder)))
                                                              (map-to-hash #'get-word-count (subfolders (parent-folder folder)))))
                                         :key-fun (compose #'simplified-path #'car))))
    (gethash (simplified-path folder) simple-path-scores)))

(defun link-score (url target-folder)
  (let ((path *classes-folder*)
        (vocab (tokens (url-text url)))
        (acc 0)
        (prob 1))
    (dolist (frag (split (subseq (simplified-path target-folder) 1) #\/))
      (if (not (equal frag ""))
          (progn
            (setf path (concat path frag "/"))
            (setf prob (* prob (prob vocab path)))
            (incf acc prob))))
    acc))

(defun pick-from-queue (queue)
  (car (first (sort (copy-seq queue) #'> :key #'cdr))))

(defun remove-wikipedia-boilerplate (link)
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
  (with-open-file (stream path
                          :direction :output :if-exists :append :if-does-not-exist :create)
    (print txt stream)))

(defun fidgetbot (seed queue-size page-count target)
  ;; TBD: Keep only one URL per domain in the queue
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
      (print "RESULTS:")
      (print results)
      (print (concat "checking " (pick-from-queue queue) ": "))
      (let* ((best-url (pick-from-queue queue))
             (raw-links (filter-links (vetted-links best-url)))
             (links (mapcar #'remove-wikipedia-boilerplate
                            (remove-duplicates (remove-if-not #'(lambda (url) (princ ".") (and (url-allowed? *crawler-name* url)
                                                                                               (not (or (equal url best-url)
                                                                                                        (member url queue :test #'equal)
                                                                                                        (member url acc :test #'equal)
                                                                                                        (search "twitter." url)
                                                                                                        (search "facebook." url)
                                                                                                        (search "youtube." url)
                                                                                                        (search "google." url)
                                                                                                        (search "amazon." url)
                                                                                                        (search "instagram." url)))))
                                                              (remove-if #'(lambda (link) (or (member (core-domain (find-domain link))
                                                                                                      visited-domains
                                                                                                      :test #'equal)
                                                                                              (equal (core-domain (find-domain link))
                                                                                                     (core-domain (find-domain best-url)))))
                                                                         raw-links))
                                               :test #'equal))))
        (push best-url acc)
        (push (core-domain (find-domain best-url)) visited-domains)
        #|(append-to-file (concat *crawl-data-folder* "focused-found")
        (concat (cdr (first (sort (copy-seq queue) #'> :key #'cdr))) " " best-url))|#
        (push (concat (cdr (first (sort (copy-seq queue) #'> :key #'cdr))) " " best-url) results)
        (setf queue (remove-nth 0 (sort (copy-seq queue) #'> :key #'cdr)))
        (print (concat "found " (write-to-string (length raw-links)) " links, of which " (write-to-string (length links)) " will be checked: "))
        (dolist (link links)
          (if (not (or (member link acc :test #'equal)
                       (member link queue :key #'car :test #'equal)))
              (let* ((worst-score (cdr (last1 queue)))
                     (score (link-score link target)))
                (if (equal (url-text link)
                           "nothingfound")
                    (princ "x")
                    (progn
                      (princ ".")
                      ;(append-to-file (concat *crawl-data-folder* "focused-all") link)
                      ;(append-to-file (concat *crawl-data-folder* "focused-all") score)
                      (if (> queue-size (length queue))
                          (setf queue (sort (copy-seq (append1 queue (cons link score))) #'> :key #'cdr))
                          (if (> score worst-score)
                              (setf queue (sort (copy-seq (replace-last queue (cons link score))) #'> :key #'cdr)))))))
              (princ "x")))))
    (reverse acc)))

(defun random-pick (lst)
  (nth (random (length lst)) lst))




(defun wanderbot (seed page-count)
  #|
  RSS feeds might be trouble. Check how they work, or finally make the downloader give up after a while.
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
