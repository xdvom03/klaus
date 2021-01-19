(defun tick-compute (master queue queue-list acc acc-list visited-domains visited-list target queue-size)
  ;; BUG: Outdated compared to the fidgetbot (text interface).
  (ltk:destroy queue-list)
  (setf queue-list (scrollable-list 0 0 master *entries-per-page* queue))
  (ltk:destroy acc-list)
  (setf acc-list (scrollable-list 0 1 master *entries-per-page* acc))
  (ltk:destroy visited-list)
  (setf visited-list (scrollable-list 0 2 master *entries-per-page* visited-domains))
  (let* ((best-url (pick-from-queue queue))
         (raw-links (filter-links (vetted-links best-url)))
         (links (remove-if #'(lambda (link) (or (member (core-domain (find-domain link))
                                                        visited-domains
                                                        :test #'equal)
                                                (equal (core-domain (find-domain link))
                                                       (core-domain (find-domain best-url)))))
                           (mapcar #'render-wikipedia
                                   (remove-duplicates (remove-if-not #'(lambda (url) (and (url-allowed? *crawler-name* url)
                                                                                          (not (or (equal url best-url)
                                                                                                   (member url queue :test #'equal)
                                                                                                   (member url acc :test #'equal)
                                                                                                   (search "twitter." url)
                                                                                                   (search "facebook." url)
                                                                                                   (search "youtube." url)
                                                                                                   (search "google." url)
                                                                                                   (search "amazon." url)
                                                                                                   (search "instagram." url)))))
                                                                     raw-links)
                                                      :test #'equal)))))
    (push best-url acc)
    (push (core-domain (find-domain best-url)) visited-domains)
    #|(append-to-file (concat *crawl-data-folder* "focused-found")
    (concat (cdr (first (sort (copy-seq queue) #'> :key #'cdr))) " " best-url))|#
    (setf queue (remove-nth 0 (sort (copy-seq queue) #'> :key #'cdr)))
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
          (princ "x")))
    (list master queue queue-list acc acc-list visited-domains visited-list target queue-size)))

(defun tick (a master queue queue-list acc acc-list visited-domains visited-list target queue-size)
  (let ((data (tick-compute master queue queue-list acc acc-list visited-domains visited-list target queue-size)))
    (print a)
    (ltk:destroy queue-list)
    (setf queue-list (scrollable-list 0 0 master *entries-per-page* queue))
    (ltk:destroy acc-list)
    (setf acc-list (scrollable-list 0 1 master *entries-per-page* acc))
    (ltk:destroy visited-list)
    (setf visited-list (scrollable-list 0 2 master *entries-per-page* visited-domains))
    (if (not (zerop a))
        (ltk:after 25 #'(lambda () (apply #'tick (1- a) data))))))

(defun run-crawler (master seed page-count target queue-size)
  ;; TBD: Run this recursively as inspired by calc.
  (let ((queue-list (scrollable-list 0 0 master *entries-per-page* nil))
        (queue (list (cons seed 0)))
        (acc-list (scrollable-list 0 1 master *entries-per-page* nil))
        (acc nil)
        (visited-list (scrollable-list 0 2 master *entries-per-page* nil))
        (visited-domains nil))
    (tick page-count master queue queue-list acc acc-list visited-domains visited-list target queue-size)))
