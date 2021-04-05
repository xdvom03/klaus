(defun wordlist (text)
  (remove-if #'(lambda (sym) (equal sym '||))
             (mapcar #'intern (cl-strings:split text))))

(defun tokens (text)
  (let* ((raw (wordlist text))
         (corp (make-hash-table :test #'eq))
         (acc (make-array *word-group-size* :initial-element nil))
         (counter 0))
    (dolist (word raw)
      (if (not (find word (subseq acc 0 counter)))
          (if (gethash word corp)
              (incf (gethash word corp))
              (setf (gethash word corp) 1)))
      (setf (aref acc counter) word)
      (setf counter (mod (1+ counter) *word-group-size*)))
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

(defun safe-check-substr (str key &optional (start 0))
  "Returns T if the string contains the key starting at start. If it is too short or doesn't contain the key, returns NIL."
  (let ((key-len (length key)))
    (and (>= (length str)
             (+ start key-len))
         (equal key (subseq str start (+ start key-len))))))

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
  (remove-tags (remove-enclosed (remove-enclosed (remove-enclosed text "<head" "</head" #\ )
                                                 "<style" "</style>" #\ )
                                "<script" "</script>" #\ )))

(defun clean-text (txt)
  (cl-strings:clean (make-safe txt)))

(defun url-text (url)
  ;; For files we merely want to download, not save (i.e. mostly just stuff in the classifier)
  (extract-text (html url)))

(defun simplify-chars (text)
  ;; Additional character fixes on top of library (which does not know many Czech diacritics)
  (map 'string #'(lambda (char) (if (equal char #\RIGHT_SINGLE_QUOTATION_MARK)
                                    #\'
                                    char))
       text))

(defun basic-text (text)
  (cl-strings:clean (make-safe (simplify-chars (cl-strings:clean-diacritics (string-downcase text))))))

(defun extract-text (html)
  (basic-text (decode-xml-entities (remove-fluff html))))


;;; TEXT CLEANING
;;;----------------------------------------------------------------------------------------------
;;; LINKS

;; QURI can call an error on (quri:uri "https://support.ted.com ") - note the space. Anything from QURI must be considered error-capable.

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
  (let ((no-resources (remove-enclosed text "<link" ">")))
    (append (find-enclosed-text no-resources "href=\"" "\"")
            (find-enclosed-text no-resources "href='" "'"))))

(defun remove-fragment (url)
  (subseq url 0 (search (concat "#" (fallback (quri:uri-fragment (quri:uri url)) "")) url)))

(defun follow-link (origin link)
  ;; This program can treat fragments as entirely irrelevant in URLs
  (remove-fragment (quri:render-uri (quri:merge-uris link origin))))

(defun vetted-links (origin html)
  ;; links may contain invalid characters, removing anything with errors is the cheapest way of handling that
  (remove-duplicates (remove-if #'null
                                (mapcar #'(lambda (link) (ignore-errors (follow-link origin link)))
                                        (find-links html)))
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

;; TBD: Introduce a settable cap on link count

;;; ROBOTS.TXT
;;;----------------------------------------------------------------------------------------------
;;; URL UTILS

(defun equivalent-urls (url1 url2)
  (equal (raw-url url1)
         (raw-url url2)))

(defun raw-url (url)
  (if (search "//" url)
      (let* ((step1 (subseq url (+ 2 (search "//" url))))
             (step2 (reverse (subseq (reverse step1)
                                     (position-if #'(lambda (character) (not (equal character #\/)))
                                                  (reverse step1))))))
        (if (equal 0 (search "www." step2))
            (subseq step2 4)
            step2))
      url))

(defun prob (vocab class)
  (if (equal class "/")
      1 ;; would crash otherwise, easiest to catch here
      (let* ((sibling-classes (classifier-options (parent-class class)))
             (path-scores (scores vocab
                                  sibling-classes
                                  (map-to-hash #'get-recursive-corpus sibling-classes)
                                  (map-to-hash #'get-word-count sibling-classes)
                                  nil)))
        (fallback (gethash class path-scores)
                  (error (concat "Class " class " does not exist."))))))

(defun valid-scheme? (url)
  (ignore-errors
   (or (equal (quri:uri-scheme (quri:uri url)) "http")
       (equal (quri:uri-scheme (quri:uri url)) "https"))))
