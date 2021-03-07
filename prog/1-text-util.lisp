;; TBD: Unite link/url/uri(?). Since we cannot use URNs, URL seems the best choice.

(defun wordlist (text)
  (mapcar #'intern (cl-strings:split text)))

(defun tokens (text)
  ;; Takes up about 10 % of corpus rebuilding time, 
  ;; TBD: Profile, check if speed actually needed
  ;; Kinda is - 10 % of total time.
  (let* ((raw (wordlist text))
         (corp (make-hash-table :test #'eq))
         (acc (make-array *word-group-size* :initial-element nil))
         (counter 0))
    (declare (type fixnum counter)
             (type fixnum *word-group-size*)
             (optimize (speed 3)))
    (dolist (word raw)
      (if (not (some #'(lambda (a) (eq a word)) acc))
          (let ((count (occurrences word corp)))
            (declare (type fixnum count))
            (if (zerop count)
                (setf (gethash word corp) 1)
                (setf (gethash word corp) (1+ count)))))
      (setf (aref acc (mod counter *word-group-size*)) word)
      (incf counter))
    corp))

;;; TOKENISATION
;;;----------------------------------------------------------------------------------------------
;;; 

(defun fast-substr-check (text key start)
  ;; TBD: There is also safe-check-substr - namees should be united, maybe both should be joined
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
  (filter text #'(lambda (a) (member a *allowed-characters*)) #\ ))

(defun decode-xml-entities (txt)
  ;; plump may cause "junk in string". Cause unknown, happens rarely, so the decoding is skipped if the error occurs.
  ;; BUG: Decodes apostrophe as RIGHT SINGLE QUOTATION MARK
  (fallback (ignore-errors (plump:decode-entities txt))
            txt))

(defun remove-tags (text)
  (remove-enclosed text "<" ">" #\ ))

(defun remove-fluff (text)
  (remove-tags (remove-enclosed (remove-enclosed text "<style" "</style>" #\ )
                                "<script" "</script>" #\ )))

(defun raw-text (url)
  (extract-raw-text (html url)))

(defun clean-text (txt)
  (cl-strings:clean (make-safe txt)))

(defun url-text (url)
  (extract-text (html url)))

(defun simplify-chars (text)
  ;; Additional character fixes on top of library (which does not know many Czech diacritics)
  (map 'string #'(lambda (char) (if (equal char #\RIGHT_SINGLE_QUOTATION_MARK)
                                    #\'
                                    char))
       text))

(defun basic-text (text)
  (simplify-chars (cl-strings:clean-diacritics (string-downcase text))))

(defun extract-text (html)
  (cl-strings:clean (make-safe (basic-text (decode-xml-entities (remove-fluff html))))))

(defun extract-raw-text (html)
  (cl-strings:clean (basic-text (decode-xml-entities (remove-fluff html)))))


;;; TEXT CLEANING
;;;----------------------------------------------------------------------------------------------
;;; LINKS

;; TBD: Let QURI do this! Also check URI resolution! (quri:render-uri (quri:merge-uris "https://en.wikipedia.org/wiki/URL" "https://en.wikipedia.org/wiki/URL"))

#|
Example: (quri:render-uri (quri:merge-uris "/wiki/Astraea" "https://en.wikipedia.org/wiki/URL"))
|#

(defun find-enclosed-text (text delim1 delim2) 
  "Lists things between delim1 and delim2 found in text. Does not include the delimiters."
  (labels ((helper (text in? acc)
             (if in?
                 (let ((loc2 (search delim2 text))
                       (len2 (length delim2)))
                   (if (search delim2 text)
                       (helper (subseq text (+ loc2 len2)) nil (append1 acc (subseq text 0 loc2)))
                       acc))
                 (let ((loc1 (search delim1 text))
                       (len1 (length delim1)))
                   (if (search delim1 text)
                       (helper (subseq text (+ loc1 len1)) t acc)
                       acc)))))
    (helper text nil nil)))

(defun find-links (text)
  ;; only looks for href
  ;; does not consider tags (could theoretically find literal text, but that seems like a non-issue)
  (append (find-enclosed-text text "href=\"" "\"")
          (find-enclosed-text text "href='" "'")))

(defun anchor-link? (link)
  ;; TBD: Is this always valid? Are there other ways to link to self?
  (and (> (length link) 0)
       (equal (char link 0) #\#)))

(defun vetted-links (url)
  (remove-duplicates (mapcar #'(lambda (link) (quri:render-uri (quri:merge-uris link url)))
                             (remove-if #'anchor-link?
                                        (find-links (html url))))
                     :test #'equal))

(defun find-domain (url)
  (quri:uri-domain (quri:uri url)))

;;; 
;;;----------------------------------------------------------------------------------------------
;;; ROBOTS.TXT
;; TBD: Fails with wildcards?

(defun remove-domain (url)
  (do ((i 0 (1+ i))
       (slashes 2 (- slashes (if (slash? (char url i)) 1 0))))
      ((or (< slashes 0)
           (>= i (length url)))
       (if (slash? (char url (1- i)))
           (subseq url (1- i))
           (subseq url i)))))
