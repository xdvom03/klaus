;; TBD: Add dollar signs, crawl delay, and asterisk wildcards.
(defun matching-rule? (rule url)
  (safe-check-substr (quri:url-decode (remove-domain url)) rule))

(defun rules (robots-txt agent-predicate)
  ;; agent-predicate is a function which determines if an agent string is considered relevant
  (apply #'append
         (mapcar #'(lambda (lst) (remove-nth 0 lst))
                 (remove-if-not #'(lambda (agents)
                                    (some #'(lambda (agent)
                                              (funcall agent-predicate agent))
                                          agents))
                                robots-txt :key #'car))))

(defun url-allowed? (bot-name url)
  (let* ((robots-txt (robots-txt url))
         (specific-rules (rules robots-txt #'(lambda (agent) (search (string-downcase bot-name)
                                                                     (string-downcase agent)))))
         (rules (fallback specific-rules (rules robots-txt #'(lambda (agents)
                                                               (some #'(lambda (agent)
                                                                         (equal "*" agent))
                                                                     agents)))))
         ;; Default to allowed
         (result t))
    ;; The first rule takes precedence
    (dolist (rule (reverse rules))
      (if (matching-rule? (cdr rule) url)
          (setf result (equal (car rule) "Allow"))))
    result))

(let ((cache (ht)))
  ;; an empty robots.txt will allow everything, which is what the spec says should be done in that case
  (defun robots-txt (site)
    ;; this link follow cannot cause an error if the site itself is OK
    (let* ((robots-txt-url (follow-link site "/robots.txt"))
           (cached (gethash robots-txt-url cache)))
      (if cached
          cached
          (let* ((file (handler-case (html robots-txt-url)
                         (error (err-text)
                           (declare (ignore err-text))
                           "")))
                 (user-agents nil)
                 (rules nil)
                 (accepting-new-agents? t)
                 (acc nil))
            (dolist (line (cl-strings:split file #\Newline))
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
            ;; also returns
            (setf (gethash robots-txt-url cache)
                  (reverse acc)))))))

(defun extension (url)
  (last1 (cl-strings:split url #\.)))

(let ((total-corp (ht)))
  (defun refresh-comprehensible-corp ()
    (setf total-corp (add-corpuses (get-recursive-corpus "/")
                                   (scale-corpus (get-recursive-corpus "/non-english/") -1))))
  
  (defun comprehensible? (vocab)
   (/ (1+ (length (remove-if #'(lambda (word) (zerop (occurrences word total-corp)))
                             vocab)))
      (1+ (length vocab)))))

(defun comprehensible-text? (raw clean)
  (/ (1+ (length clean))
     (1+ (length raw))))
