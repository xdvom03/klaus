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

(defun pick-from-queue (queue)
  (let* ((scoresum (apply #'+ (mapcar #'cdr queue)))
         (choice (random (ceiling (* scoresum 1000000))))
         (acc nil))
    (dolist (option queue)
      (decf choice (* (cdr option) 1000000))
      (if (and (null acc)
               (< choice 0))
          (setf acc (car option))))
    acc))

(defun crawl (seed queue-size page-count)
  (let ((queue (list (cons seed 1)))
        (acc nil))
    (dotimes (i page-count)
      (print (concat "QUEUE: " (write-to-string queue)))
      (let* ((best-url (pick-from-queue queue))
             (raw-links (filter-links (vetted-links best-url)))
             (links (remove-if-not #'(lambda (url) (url-allowed? *crawler-name* (print url)))
                                   (filter-links raw-links))))
        (push best-url acc)
        (setf queue (remove-nth 0 (mapcar #'(lambda (entry) (cons (car entry) (* 0.8 (cdr entry)))) ; taxing the old links
                                          (sort (copy-seq queue) #'> :key #'cdr))))
        (print (concat "found " (write-to-string (length raw-links)) " links, of which " (write-to-string (length links)) " will be checked."))
        (dolist (link links)
                                        ;(print queue)
          (print link)
                                        ;(print "okay to crawl? assume yea :D")
          (if (not (or (member link acc :test #'equal)
                       (member link queue :key #'car :test #'equal)))
              (let* ((sorted-queue (sort (copy-seq queue) #'> :key #'cdr))
                     (vocab (remove-duplicates (split (url-text link) (char " " 0)) :test #'equal))
                     (worst-score (cdr (last1 sorted-queue)))
                     (score (gethash "../DATA/dewey2/webdewey/subfolders/valuable/" (scores vocab (list "../DATA/dewey2/webdewey/subfolders/valuable/"
                                                                                                        "../DATA/dewey2/webdewey/subfolders/trash/"
                                                                                                        "../DATA/dewey2/webdewey/subfolders/boilerplate/")))))
                (if (not (equal vocab (list "nothingfound")))
                    (progn
                      (print "score:")
                      (print score)
                      (if (> queue-size (length queue))
                          (push (cons link score) queue)
                          (if (> score worst-score)
                              (setf queue (replace-last queue (cons link score))))))
                    (print "whoops, nothing here.")))))))
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
              (with-open-file (stream (concat *wanderbot-file* "2")
                                      :direction :output :if-exists :append :if-does-not-exist :create)
                (print current-url stream))
              (push (print (random-pick links)) hst)))))
    (reverse hst)))
