#|
TBD: Make concat write to string.
BUG: Queue fills up with essentially copies of the same page.
TBD: Properly cut off ttrailing section hashtags and other useless link fluff
TBD: Properly cache link scores, profile
TBD: Obey robots.txt (it's probably best for both sides)
Not using a robots.txt library for two reasons: One, no license, two, want to take everything disallowed for everybody. ACTUALLY, don'T do that, some sites block specific bots using robots.txt completely.
BUG: Unsaved comment message
TBD: Cache robots.txt (or a sensible format thereof)
BUG: Not escaped quotes, so nothing is found?
BUG: Can get stuck on: "Open too many files" or scoring "http://lightspeed.sourceforge.net/". Make it fail gracefully in these cases, releasing the queue AND acc with each major visited file (not just scored, but actually taken).
TBD: Choose randomly from the queue weighted by the scores
TBD: CACHE ROBOTS.TXT

Crawl 40 from:

"https://www.waitbutwhy.com"
"https://en.wikipedia.org"
"https://www.lifehack.org/891214/happiness-book"
"https://www.jscc.edu/academics/programs/writing-center/writing-resources/five-paragraph-essay.html"
|#

(defun remove-fragment (url)
  (do ((i 0 (1+ i)))
      ((or (>= i (length url))
           (equal (char "#" 0) (char url i))
           (equal (char "?" 0) (char url i)))
       (if (>= i (length url))
           url
           (subseq url 0 i)))))

;; TBD: Add dollar signs, crawl delay, and asterisk wildcards.
(defun matching-rule? (rule url)
  (safe-check-substr (quri:url-decode (remove-domain url)) rule))

(defun url-allowed? (bot-name url)
  (let* ((robots-txt (robots-txt url))
         (specific-rules (apply #'append
                                (mapcar #'(lambda (lst) (remove-nth 0 lst))
                                        (remove-if-not #'(lambda (agents)
                                                           (some #'(lambda (agent)
                                                                     (search (string-downcase bot-name)
                                                                             (string-downcase agent)))
                                                                 agents))
                                                       robots-txt :key #'car))))
         (rules (if specific-rules
                    specific-rules
                    (apply #'append
                           (mapcar #'(lambda (lst) (remove-nth 0 lst))
                                   (remove-if-not #'(lambda (agents)
                                                      (some #'(lambda (agent)
                                                                (equal "*" agent))
                                                            agents))
                                                  robots-txt :key #'car)))))
         ;; Default to allowed
         (result t))
    ;; The first rule takes precedence
    (dolist (rule (reverse rules))
      (if (matching-rule? (cdr rule) url)
          (setf result (equal (car rule) "Allow"))))
    result))

(defun safe-check-substr (str key &optional (start 0))
  "Returns T if the string contains the key starting at start. If it is too short or doesn't contain the key, returns NIL."
  (let ((key-len (length key)))
    (and (>= (length str)
             (+ start key-len))
         (equal key (subseq str start (+ start key-len))))))

(defun robots-txt (site)
  (let* ((file (safe-fetch-html (concat (find-domain site) "/robots.txt")))
         (user-agents nil)
         (rules nil)
         (accepting-new-agents? t)
         (acc nil))
    (dolist (line (split file (char *newline* 0)))
      (let* ((agent? (safe-check-substr line "User-agent: "))
             (allow? (safe-check-substr line "Allow: "))
             (disallow? (safe-check-substr line "Disallow: "))
             (line-body (subseq line (length (cond (agent? "User-agent: ")
                                                   (allow? "Allow: ")
                                                   (disallow? "Disallow: ")
                                                   (t ""))))))
        (cond (agent?
               (if accepting-new-agents?
                   (push line-body user-agents)
                   (progn
                     (if user-agents
                         (push (cons user-agents rules) acc))
                     (setf user-agents (list line-body))
                     (setf rules nil))))
              (allow?
               (push (cons "Allow" line-body) rules))
              (disallow?
               (push (cons "Disallow" line-body) rules)))
        (setf accepting-new-agents? agent?)))
    (push (cons user-agents rules) acc)
    (reverse acc)))

(defun extension (url)
  (car (last (split url (char "." 0)))))

(defun filter-links (raw-links)
  (remove-if #'(lambda (url)
                 (member (extension url)
                         *forbidden-extensions*
                         :test #'equal))
             (remove-duplicates (mapcar #'remove-fragment
                                        raw-links))))

(defun crawl (seed queue-size page-count)
  (let ((queue (list (cons seed 1)))
        (acc nil))
    (dotimes (i page-count)
      (print (concat "QUEUE: " (write-to-string queue)))
      (let* ((best-url (car (first (sort (copy-seq queue) #'> :key #'cdr))))
             (raw-links (filter-links (vetted-links best-url
                                                    (find-domain best-url))))
             (message (print (concat "found " (write-to-string (length raw-links)) " links. Checking:")))
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
                     (worst-score (cdr (car (last sorted-queue))))
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
                              (setf queue (let ((acc sorted-queue))
                                            (setf (car (last acc))
                                                  (cons link score))
                                            acc)))))
                    (print "whoops, nothing here.")))))))
    (reverse acc)))
