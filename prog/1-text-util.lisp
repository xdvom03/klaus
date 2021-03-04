;; BUG: RIGHT SINGLE QUOTATION MARK should be an apostrophe! Also, library does not know some Czech diacritics.
;; TBD: Unite link/url/uri(?)

(defun find-enclosed-text (text delim1 delim2 &optional (space nil))
  "Lists things between delim1 and delim2 found in text. Does not include the delimiters. Only returns the first word according to an optional parameter. If it's nil, it returns the whole delimited part. Ignores case." 
  (let ((acc nil)
        (unit nil))
    (dotimes (i (1- (length text)))
      (if (and (<= (+ i (length delim1))
                   (length text))
               (string-equal (string-downcase (subseq text i (+ i (length delim1))))
                             (string-downcase delim1)))
          (progn
            (do ((j (length delim1) (1+ j)))
                ((or (>= (+ i j (length delim2)) (length text))
                     (string-equal (string-downcase (subseq text (+ i j) (+ i j (length delim2))))
                                   (string-downcase delim2)))
                 (if (>= (+ i j (length delim2)) (length text))
                     (setf unit 0))) ;ignores entire unit
              (push (char text (+ i j)) unit))
            (if (not (equal unit 0))
                (let ((unit-str (if space
                                    (first (cl-strings:split (convert-to-str (reverse unit))))
                                    (convert-to-str (reverse unit)))))
                  (if (not (member unit-str acc :test #'equal :key #'string-downcase))
                      (push unit-str acc))))
            (setf unit nil))))
    (reverse acc)))

(defun wordlist (text)
  (mapcar #'intern (cl-strings:split text)))

(defun tokens (text)
  (let* ((raw (wordlist text))
         (corp (make-hash-table :size (length raw) :test #'eq))
         (acc (make-array *word-group-size* :initial-element nil))
         (counter 0)
         (weight 1) ;; TEMP: this will become a folder weight
         )
    (dolist (word raw)
      (if (not (some #'(lambda (a) (eq a word)) acc))
          (if (gethash word corp)
              (incf (gethash word corp) weight)
              (setf (gethash word corp) weight)))
      (setf (aref acc (mod counter *word-group-size*)) word)
      (incf counter))
    corp))

;;; 
;;;----------------------------------------------------------------------------------------------
;;;

(defun fast-substr-check (text key start)
  ;; First checks whether the first character matches, eliminating most incorrect guesses
  (let ((key-len (length key))
        (text-len (length text)))
    (and (< (+ start key-len) text-len)
         (equal (char text start) (char key 0))
         (equal (subseq text start (+ start key-len)) key))))

;;; 
;;;----------------------------------------------------------------------------------------------
;;; TEXT CLEANING

(defun remove-enclosed (text delim1 delim2 &optional swap-for)
  "Removes all text enclosed between delim1 and delim2, including the tags. May replace with a character instead. Returns remaining text. Nondestructive."
  (let ((acc nil)
        (enclosed-p nil))
    (dotimes (i (length text))
      (cond ((fast-substr-check text delim1 i)
             (setf enclosed-p t)
             (if swap-for
                 (if (characterp swap-for)
                     (push swap-for acc)
                     (setf acc
                           (append (reverse (cl-strings:chars swap-for)) acc)))))
            ((fast-substr-check text delim2 i)
             (if (not enclosed-p)
                 (push (char text i) acc))
             (setf enclosed-p nil)
             (setf i (+ i (length delim2) -1)))
            ((not enclosed-p)
             (push (char text i) acc))))
    (convert-to-str (reverse acc))))

(defun filter (text allow-rule &optional censor-by)
  (let ((acc nil))
    (dotimes (i (length text))
      (if (funcall allow-rule (char text i))
          (push (char text i) acc)
          (if censor-by (push censor-by acc))))
    (convert-to-str (reverse acc))))

(defun make-safe (text)
  (filter text #'(lambda (a) (member a *allowed-characters*))))

(defun decode-xml-entities (txt)
  ;; plump may cause "junk in string". Cause unknown, happens rarely, so the decoding is skipped if the error occurs.
  (fallback (ignore-errors (plump:decode-entities txt))
            txt))

;;; TEXT CLEANING
;;;----------------------------------------------------------------------------------------------
;;;

;; TBD: Let QURI do this! Also check URI resolution! (quri:render-uri (quri:merge-uris "https://en.wikipedia.org/wiki/URL" "https://en.wikipedia.org/wiki/URL"))

#|
Example: (quri:render-uri (quri:merge-uris "/wiki/Astraea" "https://en.wikipedia.org/wiki/URL"))
|#

(defun protocol (url) ;; TBD: Scheme, not protocol?
  (let ((pos (position #\: url)))
    (if pos
        (subseq url 0 pos))))

(defun find-links (text)
  ;; only looks for href
  ;; does not consider tags (could theoretically find literal text, but that seems like a non-issue)
  (append (find-enclosed-text text "href=\"" "\"")
          (find-enclosed-text text "href='" "'")))

(defun absolute-link? (link)
  (and (> (length link) 3)
       (equal (subseq link 0 4) "http")))

(defun relative-protocol-link? (link)
  (and (> (length link) 1)
       (equal (subseq link 0 2) "//")))

(defun anchor-link? (link)
  (and (> (length link) 0)
       (equal (char link 0) #\#)))

(defun extract-link (link domain)
  ;; Empty links happen sometimes, as do super short ones, should be removed
  ;; Anything not starting with http is assumed to be a relative link, starting slash removed, domain name prepended
  (if (absolute-link? link)
      link
      ;; double slash is relative protocol
      (if (relative-protocol-link? link)
          (concat (protocol link)
                  link)
          (concat domain
                  link))))

(defun vetted-links (url)
  (let ((domain (find-domain url)))
    (remove-duplicates (mapcar #'(lambda (link) (extract-link link domain))
                               (remove-if #'anchor-link?
                                          (find-links (safe-fetch-html url))))
                       :test #'equal)))

(defun find-domain (url)
  (do ((i 0 (1+ i))
       (slashes 2 (- slashes (if (slash? (char url i)) 1 0))))
      ((or (< slashes 0)
           (>= i (length url)))
       (subseq url 0 (if (< slashes 0) ; if found a third slash, cut it, but if it reached the end, keep the whole domain
                         (1- i)
                         i)))))

(defun remove-domain (url)
  (do ((i 0 (1+ i))
       (slashes 2 (- slashes (if (slash? (char url i)) 1 0))))
      ((or (< slashes 0)
           (>= i (length url)))
       (if (slash? (char url (1- i)))
           (subseq url (1- i))
           (subseq url i)))))

(defun remove-tags (text)
  (remove-enclosed text "<" ">" #\ ))

(defun remove-fluff (text)
  (remove-tags (remove-enclosed (remove-enclosed text
                                                 "<style" "</style>" #\ )
                                "<script" "</script>" #\ )))

(defun raw-text (url)
  (extract-raw-text (safe-fetch-html url)))

(defun clean-text (txt)
  (cl-strings:clean (make-safe txt)))

(defun url-text (url)
  ;; Fetches text of a url
  (extract-text (safe-fetch-html url)))

(defun basic-text (text)
  (cl-strings:clean-diacritics (string-downcase text)))

(defun extract-text (html)
  (cl-strings:clean (make-safe (basic-text (decode-xml-entities (remove-fluff html))))))

(defun extract-raw-text (html)
  (cl-strings:clean (basic-text (decode-xml-entities (remove-fluff html)))))
