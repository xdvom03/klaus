(ql:quickload (list "ltk" "dexador" "quri"))

;; TBD: Unite the "data retrieval from file" process

;;; IMPORTS
;;;----------------------------------------------------------------------------------------------
;;; UTILS

(defparameter *classes-folder* "../DATA/klaus/classes/")
(defparameter *files-folder* "../DATA/klaus/files/")
(defparameter *aliases-file* "../DATA/klaus/file-aliases")
;; BEWARE: Do not change without knowing what you are doing! Can mess up history!
;; TBD: Turn this into another folder
(defparameter *history-file* "../DATA/klaus/history/history")
(defparameter *history-temp-file* "../DATA/klaus/history/history2")
(defparameter *history-rename* "history")

(defparameter *entries-per-page* 20)
(defparameter *try-to-class?* t)
(defparameter *explain?* nil)
(defparameter *evidence-length* 6)
(defparameter *newline* "
")
(defparameter *iterations* 20)
(defparameter *crawler-name* "botelaire")

(defparameter *forbidden-extensions* (list "css" "png" "mp4" "ico" "svg" "webmanifest"))

(defun pass ())

(defun append1 (lst elem)
  (append lst (list elem)))

(defun shuffle (lst)
  (let ((acc1 nil)
        (acc2 lst))
    (dotimes (i (length lst))
      (let ((index (random (length acc2))))
        (push (nth index acc2) acc1)
        (setf acc2 (remove-nth index acc2))))
    acc1))

(defun remove-nth (n lst)
  (append (subseq lst 0 n) (subseq lst (1+ n))))

(defun remove-last (lst)
  (reverse (cdr (reverse lst))))

(defun slash? (char)
  (equal char (char "/" 0)))

(defun folder? (path)
  (slash? (char (namestring path) (1- (length (namestring path))))))

(defun countup (num)
  ;; TBD: Check that this is used legitimately, perhaps a mapa->b like function is what we're really after.
  ;; Returns a list of length num with integers incrementing from zero
  (let ((acc nil))
    (dotimes (i num)
      (push i acc))
    (reverse acc)))

(defun convert-to-str (list)
  (concatenate 'string list))

(defun concat (&rest strings)
  ;; Writes numbers out, but leaves the rest be to signal errors if something VERY wrong is supplied
  ;; Not allowing lists for now because it seems like more trouble than it's worth
  (apply #'concatenate 'string (mapcar #'(lambda (a)
                                           (if (null a) (error "Trying to concat with NIL")) ; specific case, signal first
                                           (if (listp a) (error "Trying to concat a list, use convert-to-str instead!"))
                                           (if (numberp a)
                                               (write-to-string a)
                                               a))
                                       strings)))

;;; UTILS
;;;----------------------------------------------------------------------------------------------
;;; FILE READING

(defun file-lines (file)
  ;; UIOP for some reason always reads a blank first line
  ;; List of string lines
  (cdr (uiop:read-file-lines file)))

(defun read-from-file (path)
  (with-open-file (stream path :direction :input)
    (read stream)))

(defun create-file (path content)
  (with-open-file (stream path :direction :output :if-does-not-exist :create)
    (print content stream)))

;;; FILE READING
;;;----------------------------------------------------------------------------------------------
;;; FOLDER NAVIGATION

(defun directory* (folder)
  (mapcar #'namestring (directory (concat folder "*"))))

(defun read-comment (folder)
  (if (directory (concat folder "comment"))
      (with-open-file (stream (concat folder "comment") :direction :input)
        (read stream))
      (set-comment "" folder)))

(defun set-comment (text folder)
  (with-open-file (stream (concat folder "comment") :direction :output :if-exists :supersede :if-does-not-exist :create)
    (print text stream)))

;; TBD: All we need to fix for rebuild-corpus to work again should be the plural functions here
(defun subfolders (folder)
  (remove-if-not #'folder? (directory* folder)))

(defun file-url (file) ;; Beware, this is required for link window! Change that!
  (read-from-file (concat (namestring file) "url")))

(defun file-urls (class) ;; TBD: Obviously a bit pointless
  (class-links class))

(defun file-text (file-name)
  (extract-text (file-content file-name)))

(defun file-texts (folder)
  (mapcar #'file-text (class-links folder)))

(defun simplified-path (path)
  (let* ((path-parts (split path (char "/" 0)))
         (varying-part (cdr (member "classes" path-parts :test #'equal))))
    (reduce #'(lambda (a b) (concat a "/" b))
            ;; Prepend an empty string in case of the home folder - reducing NIL it has no idea what the base state is.
            (cons "" varying-part))))

;;; FOLDER NAVIGATION
;;;----------------------------------------------------------------------------------------------
;;; WIDGET UTILS

(defun widget (r c type master)
  (let ((w (make-instance type :master master)))
    (ltk:grid w r c :sticky "nesw")
    (ltk:grid-rowconfigure master r :weight 1)
    (ltk:grid-columnconfigure master c :weight 1)
    w))

(defun button (r c master txt command)
  (let ((b (widget r c 'ltk:button master)))
    (setf (ltk:text b) txt)
    (setf (ltk:command b) command)
    b))

(defun button-column (window column page-length &optional (starting-row 0))
  (let ((acc nil))
    (dotimes (i page-length)
      (push (button (+ i starting-row) column window "" #'pass) acc))
    (reverse acc)))

(defun text (r c master txt)
  (let ((tex (widget r c 'ltk:text master)))
    (setf (ltk:text tex) txt)
    tex))

(defun listbox (r c master txt)
  (let ((l (widget r c 'ltk:listbox master)))
    (setf (ltk:text l) txt)
    l))

(defun entry (r c master)
  (widget r c 'ltk:entry master))

(defun checkbox (r c master text command)
  (let ((ch (widget r c 'ltk:check-button master)))
    (setf (ltk:command ch) command)
    (setf (ltk:text ch) text)
    ch))

(defun label (r c master txt)
  (let ((l (widget r c 'ltk:label master)))
    (setf (ltk:text l) txt)
    (ltk:configure l :anchor :center)
    l))

(defun frame (r c master)
  (widget r c 'ltk:frame master))

(defun window (title)
  (make-instance 'ltk:toplevel :title title))

(defun list-window (lst function-lst page-length title)
  (let* ((start 0)
         (W (window title))
         (f (frame (length lst) 0 W))
         (l (label (1+ (length lst)) 0 W ""))
         (buttonlist (button-column W 0 page-length))
         left
         right
         (redraw #'(lambda ()
                     (dotimes (i page-length)
                       (let ((b (nth i buttonlist)))
                         (if (> (length lst) (+ start i) -1)
                             (let ((txt (nth (+ start i) lst))
                                   (command (nth (+ start i) function-lst)))
                               (setf (ltk:text b) txt)
                               (setf (ltk:command b) command))
                             (progn
                               (setf (ltk:text b) "")
                               (setf (ltk:command b) #'pass)))))
                     (setf (ltk:text l) (concat start "/" (length lst)))
                     (if (>= start page-length)
                         (ltk:configure left :state :normal)
                         (ltk:configure left :state :disabled))
                     (if (<= start (- (length lst) page-length))
                         (ltk:configure right :state :normal)
                         (ltk:configure right :state :disabled)))))
    (setf left (button 0 0 f "←" #'(lambda ()
                                     (decf start page-length)
                                     (funcall redraw))))
    (setf right (button 0 1 f "→" #'(lambda ()
                                      (incf start page-length)
                                      (funcall redraw))))
    (funcall redraw)
    W))

;;; WIDGET UTILS
;;;----------------------------------------------------------------------------------------------
;;; HTML & LINKS

(defun open-url (url)
  "Open the URL in the browser. Waits a bit for it to load."
  (uiop:run-program (format nil "xdg-open ~S" url))
  (sleep 1))

(defun split (text char)
  "Returns a list of parts of the text denoted by the character. Removes the splitting character."
  (let ((word-acc nil)
        (text-acc nil))
    (dotimes (i (length text))
      (if (equal (char text i) char)
          (progn
            (push (reverse text-acc) word-acc)
            (setf text-acc nil))
          (push (char text i) text-acc)))
    (mapcar #'(lambda (a) (convert-to-str a)) ; needed to turn character lists into words
            (reverse (cons (reverse text-acc) word-acc)))))

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
                                    (first (split (convert-to-str (reverse unit)) #\ ))
                                    (convert-to-str (reverse unit)))))
                  (if (not (member unit-str acc :test #'equal :key #'string-downcase))
                      (push unit-str acc))))
            (setf unit nil))))
    (reverse acc)))

(defun find-links (text)
  ;; ignores <a> tags, since the only relevant part of them is the href. 
  (append (find-enclosed-text text "href=\"" "\"")
          (find-enclosed-text text "href='" "'")))

(defun safe-fetch-html (url)
  "Gets HTML data from a URL, but if it 404's, it returns nothing found."
  ;; TBD: Figure out how to dynamically insert the bot name into the headers
  (let ((unsafe (ignore-errors (dex:get url :headers '(("User-Agent" . "Botelaire, crawler for https://github.com/xdvom03/klaus (reads robots.txt for botelaire). In case of any trouble, contact xdvom03 [at] gjk [dot] cz."))))))
    ;; an image link, or anything that isn't a string, is considered a 404
    (if (or (null unsafe) (not (stringp unsafe)))
        "nothingfound"
        unsafe)))

(defun url-links (url)
  "Returns a list of all links found in url"
  (find-links (safe-fetch-html url)))

(defun extract-link (link domain)
  ;; Empty links happen sometimes, as do super short ones, should be removed
  ;; TBD: Deal properly with various forms of relative links (fails with Tailsteak site). For now, keep old slash version.
  ;; Anything not starting with http is assumed to be a relative link, starting slash removed, domain name appended
  (if (and (> (length link) 0)
           (slash? (char link 0)))
      (concat domain
              link)
      link))

(defun vetted-links (url domain)
  (remove-if-not #'(lambda (str) (and (> (length str) 0)
                                      (equal (char str 0) (char "h" 0))))
                 (mapcar #'(lambda (tag) (extract-link tag domain)) (url-links url))))

(defun find-domain (url)
  (do ((i 0 (1+ i))
       (slashes 2 (- slashes (if (slash? (char url i)) 1 0))))
      ((or (< slashes 0)
           (>= i (length url)))
       (if (slash? (char url (1- i)))
           (subseq url 0 (1- i))
           (subseq url 0 i)))))

(defun remove-domain (url)
  (do ((i 0 (1+ i))
       (slashes 2 (- slashes (if (slash? (char url i)) 1 0))))
      ((or (< slashes 0)
           (>= i (length url)))
       (if (slash? (char url (1- i)))
           (subseq url (1- i))
           (subseq url i)))))

(defun remove-enclosed (text delim1 delim2)
  ;; TBD: Verify
  "Removes all text enclosed between delim1 and delim2, including the tags. Returns remaining text. Nondestructive."
  (let ((acc nil)
        (enclosed-p nil))
    (dotimes (i (length text))
      (cond ((equal (subseq text i (min (+ i (length delim1)) (length text))) delim1) (setf enclosed-p t))
            ((equal (subseq text i (min (+ i (length delim2)) (length text))) delim2) (progn
                                                                                        (setf enclosed-p nil)
                                                                                        (setf i (+ i (length delim2) -1))))
            (t (if (not enclosed-p)
                   (push (char text i) acc)))))
    (convert-to-str (reverse acc))))

(defun remove-tags (text)
  (remove-enclosed text "<" ">"))

;;; HTML & LINKS
;;;----------------------------------------------------------------------------------------------
;;; DATA DOWNLOAD

(defun make-safe (text)
  ;;Removes characters that cause trouble when printing
  ;;Replaces them with stars
  (let ((acc nil))
    (dotimes (i (length text) (reverse acc))
      (if (member (char text i) '(#\  #\EM_DASH #\LEFTWARDS_ARROW #\EN_DASH #\PLUS-MINUS_SIGN #\THIN_SPACE #\MINUS_SIGN #\' #\DEGREE_SIGN #\# #\6 #\Tab #\_ #\7 #\% #\& #\? #\+ #\@ #\} #\Z #\X #\M #\z #\H #\q #\V #\K #\J #\y #\x #\B
                                  #\* #\G #\A #\9 #\4 #\5 #\3 #\I #\v #\0 #\b #\S #\: #\{ #\f #\] #\[ #\Q #\L #\R #\w #\; #\2 #\1 #\, #\$ #\) #\\ #\| #\^ #\( #\N #\. #\u #\p #\k #\W #\/ #\8 #\F #\U #\r #\d #\g #\j #\o #\- #\n #\e #\i
                                  #\" #\= #\s #\a #\c #\> #\l #\m #\t #\h #\E #\P #\Y #\T #\C #\O #\D #\! #\< #\  #\Newline))
          (push (char text i) acc)
          (push (char "*" 0) acc)))
    (convert-to-str (reverse acc))))

(defun remove-punctuation (text)
  ;; Replaces commas, parentheses, periods, etc. by spaces
  (let ((acc nil))
    (dotimes (i (length text))
      (if (member (char text i) '(#\EM_DASH #\LEFTWARDS_ARROW #\EN_DASH #\PLUS-MINUS_SIGN #\THIN_SPACE #\MINUS_SIGN #\DEGREE_SIGN #\# #\& #\? #\+ #\}
                                  #\* #\: #\{ #\] #\[ #\; #\, #\) #\\ #\| #\^ #\( #\. #\/ #\-
                                  #\" #\= #\> #\! #\<))
          (push (char " " 0) acc)
          (push (char text i) acc)))
    (convert-to-str (reverse acc))))

(defun remove-whitespace (text)
  ;;Keeps spaces but removes tabs, newlines, etc.
  ;;Replaces them by spaces
  (let ((acc nil))
    (dotimes (i (length text))
      (if (member (char text i) '(#\  #\EM_DASH #\LEFTWARDS_ARROW #\EN_DASH #\PLUS-MINUS_SIGN #\THIN_SPACE #\MINUS_SIGN #\' #\DEGREE_SIGN #\# #\6  #\_ #\7 #\% #\& #\? #\+ #\@ #\} #\Z #\X #\M #\z #\H #\q #\V #\K #\J #\y #\x #\B
                                  #\* #\G #\A #\9 #\4 #\5 #\3 #\I #\v #\0 #\b #\S #\: #\{ #\f #\] #\[ #\Q #\L #\R #\w #\; #\2 #\1 #\, #\$ #\) #\\ #\| #\^ #\( #\N #\. #\u #\p #\k #\W #\/ #\8 #\F #\U #\r #\d #\g #\j #\o #\- #\n #\e #\i
                                  #\" #\= #\s #\a #\c #\> #\l #\m #\t #\h #\E #\P #\Y #\T #\C #\O #\D #\! #\< #\ ))
          (push (char text i) acc)
          (push (char " " 0) acc)))
    (convert-to-str (reverse acc))))

(defun remove-diacritics (str)
  (map 'string
       #'basic-letter
       str))

(defun basic-letter (ltr)
  (let ((char-pairs (list (list "á" "a")
                          (list "é" "e")
                          (list "ě" "e")
                          (list "í" "i")
                          (list "ó" "o")
                          (list "ú" "u")
                          (list "ů" "u")
                          (list "ý" "y")
                          (list "č" "c")
                          (list "ď" "d")
                          (list "ň" "n")
                          (list "ř" "r")
                          (list "š" "s")
                          (list "ť" "t")
                          (list "ž" "z")))
        (acc nil))
    (dolist (pair char-pairs)
      (if (equal ltr (char (first pair) 0))
          (setf acc (char (second pair) 0))))
    (if acc
        acc
        ltr)))

(defun links (file-name)
  (with-open-file (stream file-name :direction :input)
    (let ((str (make-string (file-length stream))))
      (read-sequence str stream)
      (mapcar #'(lambda (str)
                  (subseq str 1 (- (length str) 2)))
              (remove-if #'(lambda (str) (equal str ""))
                         (split str #\Newline))))))

;;; DATA DOWNLOAD
;;;----------------------------------------------------------------------------------------------
;;; CORPUS

(defun occurrences (word corpus)
  (let ((corpus-entry (gethash word corpus)))
    (if corpus-entry
        corpus-entry
        0)))

(defun wordlist (text)
  ;; TBD: Fix name (hashtable!)
  ;; Produces a "corpus" (with each word in there once) out of a text. Hash table format,
  (let ((lst (remove-duplicates (split text (char " " 0)) :test #'equal))
        (corpus (make-hash-table :test #'equal)))
    (setf (gethash nil corpus) lst)
    (dolist (word lst)
      (setf (gethash word corpus) 1))
    corpus))

(defun normalize-corpus (corp num words)
  ;; Scales the hash table format by a factor of num. Only carries over "words", not the full corpus.
  (let* ((acc (make-hash-table :test #'equal)))
    (dolist (word words)
      (setf (gethash word acc) (* num (occurrences word corp))))
    (setf (gethash nil acc) words)
    acc))

(defun rebuild-corpus (&optional (folder *classes-folder*))
  ;; Writes the cons list format of the corpuses into the respective files.
  (labels ((print-to-file (file-name &rest things)
             (with-open-file (stream (concat folder file-name) :direction :output :if-exists :supersede :if-does-not-exist :create)
               (dolist (i things)
                 (print i stream)))))
    
    (let ((subfolders (subfolders folder))
          (url-count 0)
          (corpus (make-hash-table :test #'equal))
          (timer (get-internal-real-time)))
      
      (dolist (subfolder subfolders)
        (let ((data (rebuild-corpus subfolder)))
          (incf url-count (car data))
          (setf corpus (add-hashtable-corpuses corpus (cdr data)))))
      
      (let* ((data (get-corpus folder)))
        (incf url-count (car data))
        (setf corpus (add-hashtable-corpuses corpus (cdr data))))

      (print-to-file "file-count" url-count)
      (print-to-file "corpus" (corpus-list corpus))
      (if (equal folder *classes-folder*)
          (log-print "Rebuilt the corpus. Time taken: "
                     ;; internal-time-units-per-second is a LISP built-in constant
                     (coerce (/ (- (get-internal-real-time) timer) internal-time-units-per-second) 'single-float)))
      (cons url-count corpus))))

(defun add-hashtable-corpuses (corp1 corp2)
  ;; Works with the hashtable format
  (let ((acc (make-hash-table :test #'equal))
        (wordlist (remove-duplicates (append (gethash nil corp1)
                                             (gethash nil corp2))
                                     :test #'equal)))
    (dolist (word wordlist)
      (setf (gethash word acc)
            (+ (occurrences word corp1)
               (occurrences word corp2))))
    (setf (gethash nil acc) wordlist)
    acc))

(defun sort-corpus (corp)
  ;; TBD: Flagged for possible deletion.
  ;; Only applicable for the list format of corpuses.
  (sort (copy-seq corp) #'< :key #'cdr))

(defun get-corpus (folder)
  ;; Returns cons of url count & corpus
  (let ((vocab-lists (mapcar #'wordlist
                             (file-texts folder))))
    (cons (length vocab-lists)
          (if vocab-lists
              (reduce #'add-hashtable-corpuses
                      vocab-lists)
              ;; Create an empty hash table
              (make-hash-table :test #'equal)))))

(defun corpus-hashtable (list)
  ;; Converts from the list format to the hash table format
  ;; NIL contains a list of all hashes to be used when adding or subtracting.
  (let ((corpus (make-hash-table :test #'equal)))
    (dolist (word list)
      (setf (gethash (car word) corpus) (cdr word)))
    (setf (gethash nil corpus) (mapcar #'car list))
    corpus))

(defun corpus-list (hashtable)
  ;; Converts from the hash table format to the list format
  (let ((corpus nil))
    (dolist (word (gethash nil hashtable))
      (push (cons word (occurrences word hashtable)) corpus))
    corpus))

(defun get-recursive-corpus (folder)
  ;; Looks into the corpus file and converts to the hash-table formulation
  (with-open-file (stream (concat folder "corpus"))
    (corpus-hashtable (read stream))))

(defun get-subfolder-corpus (folder)
  ;; Like get-recursive-corpus, but without the files in the folder itself.
  (corpus-subtract (get-recursive-corpus folder)
                   (cdr (get-corpus folder))))

(defun corpus-subtract (corp1 corp2)
  ;; Works with hash tables
  ;; Assumes that corp2 is a subcorpus of corp2, so there is nothing negative in the result (and the vocabulary of corp2 is included in the vocabulary of corp1)
  ;; No need to remove words with 0 occurrences. TBD: In case of bottleneck, check if it makes the program faster.
  (let ((acc (make-hash-table :test #'equal)))
    (dolist (word (gethash nil corp1))
      (setf (gethash word acc)
            (- (occurrences word corp1)
               (occurrences word corp2))))
    (setf (gethash nil acc) (gethash nil corp1))
    acc))

(defun get-file-count (folder)
  ;; Just looks into the file-count file
  (with-open-file (stream (concat folder "file-count"))
    (read stream)))

;;; CORPUS
;;;----------------------------------------------------------------------------------------------
;;; DATABASE

(defun parent-folder (folder-path)
  (do ((i (- (length folder-path) 2) (1- i)))
      ((slash? (char folder-path i))
       (subseq folder-path 0 (1+ i)))))

(defun file-name (path folder?)
  (do ((i (if folder?
              (- (length path) 2)
              (1- (length path)))
          (1- i)))
      ((slash? (char path i))
       (subseq path (1+ i)))))

(defun extract-text (html)
  (let* ((safe (make-safe (remove-diacritics (string-downcase html))))
         (content (remove-whitespace (remove-tags (remove-enclosed (remove-enclosed safe "<style" "</style>") "<script" "</script>")))))
    content))

;;; DATABASE
;;;----------------------------------------------------------------------------------------------
;;; HISTORY

(defun add-to-history (url)
  (with-open-file (stream *history-file* :direction :output :if-exists :append :if-does-not-exist :create)
    (print url stream)))

(defun history ()
  (mapcar #'read-from-string (file-lines *history-file*)))

(defun remove-from-history (index)
  (log-print "Removing " (nth index (history)) " from history.")
  (let ((hst (history)))
    (with-open-file (stream *history-temp-file* :direction :output :if-exists :supersede :if-does-not-exist :create)
      (dolist (i (remove-nth index hst))
        (print i stream)))
    (delete-file *history-file*)
    (rename-file *history-temp-file* *history-rename*)))

(defun history-window (url-entry page-length)
  (let* ((W (window "History"))
         (e (entry 0 3 W))
         (f (frame page-length 1 W))
         (l (label (1+ page-length) 1 W ""))
         (buttons (button-column W 1 page-length))
         (buttons2 (button-column W 2 page-length))
         (start 0)
         left
         right)
    (labels ((redraw ()
               (let ((hst (history)))
                 (dotimes (i page-length)
                   (let ((open-link (+ start i)) ;rebind
                         (b (nth i buttons)))
                     (if (> (length hst) (+ start i) -1)
                         (progn
                           (setf (ltk:text b) (nth (+ start i) hst))
                           (setf (ltk:command b) #'(lambda () (setf (ltk:text url-entry) (nth open-link hst)))))
                         (progn
                           (setf (ltk:text b) "")
                           (setf (ltk:command b) #'pass)))))
                 (dotimes (i page-length)
                   (let ((remove-link (+ start i)) ; rebind
                         (b (nth i buttons2)))
                     (if (> (length hst) (+ start i) -1)
                         (progn
                           (setf (ltk:text b) "REMOVE")
                           (setf (ltk:command b) #'(lambda ()
                                                     (remove-from-history remove-link)
                                                     (redraw))))
                         (progn
                           (setf (ltk:text b) "")
                           (setf (ltk:command b) #'pass)))))
                 (setf (ltk:text l) (concat start "/" (length hst)))
                 (if (>= start page-length)
                     (ltk:configure left :state :normal)
                     (ltk:configure left :state :disabled))
                 (if (<= start (- (length hst) page-length))
                     (ltk:configure right :state :normal)
                     (ltk:configure right :state :disabled)))))
      (button 0 0 W "X" #'kill-all)
      (setf left (button 0 0 f "←" #'(lambda ()
                                       (decf start page-length)
                                       (redraw))))
      (setf right (button 0 1 f "→" #'(lambda ()
                                        (incf start page-length)
                                        (redraw))))
      (redraw)
      (button 1 3 W "Push to history" #'(lambda ()
                                          (add-to-history (ltk:text e)) (redraw))))))

;;; HISTORY
;;;----------------------------------------------------------------------------------------------
;;; SCORE MATH

(defun pagerank (options scores-table)
  ;; Pageranking one or zero folders results in division 0/0
  (if (< (length options) 2)
      (let ((scores (make-hash-table :test #'equal)))
        (dolist (option options)
          (setf (gethash option scores) 1))
        scores)
      (let ((scores (make-hash-table :test #'equal))
            (acc (make-hash-table :test #'equal)))
        (dolist (option options)
          (setf (gethash option scores)
                ;; BEWARE of ratios entering this calculation, for they explode in precision, size, and lag.
                (coerce (/ (length options)) 'single-float)))
        (dotimes (i *iterations*)
          (dolist (option options)
            ;; BUG: Possible division by 0. Limit probabilities to double float range.
            (setf (gethash option acc)
                  (/ (apply #'+
                            (mapcar #'(lambda (option2)
                                        (if (equal option option2)
                                            0
                                            (* (gethash option2 scores)
                                               (gethash (cons option option2) scores-table))))
                                    options))
                     (apply #'+ (mapcar #'(lambda (option2) (if (equal option2 option)
                                                                1d-15 ; prevents division by zero if it is certain the document belongs in a given category
                                                                (gethash option2 scores)))
                                        options)))))
          (let ((probsum (apply #'+ (mapcar #'(lambda (opt) (gethash opt acc))
                                            options))))
            ;; TBD: Maybe somehow display the eventual probsum, because it is also a result of the calculation, it is unique, and seems to somehow reflect on the certainty of the result
            ;; Probsum: Min 1, Max N/2 for N options
            (dolist (option options)
              (setf (gethash option scores)
                    (/ (gethash option acc)
                       probsum)))))
        scores)))

;; Using Laplace smoothing for now
(defun word-probability (target total subfolder-count)
  (/ (+ 1 target)
     (+ subfolder-count total)))
