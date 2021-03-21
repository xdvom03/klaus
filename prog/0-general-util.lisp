(ql:quickload (list "ltk" "drakma" "quri" "plump" "trivial-timeout" "cl-strings"))

;;; IMPORTS
;;;----------------------------------------------------------------------------------------------
;;; CONFIG VARIABLE INIT

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

;; paths
(defparameter *classes-folder* "../DATA/classes/")
(defparameter *generated-data-folder* "../DATA/generated-data/")
(defparameter *imports-folder* "../DATA/imports/")
(defparameter *files-folder* "../DATA/files/")
(defparameter *html-folder* (concat *files-folder* "html/"))
(defparameter *text-folder* (concat *files-folder* "text/"))
(defparameter *raw-folder* (concat *files-folder* "raw/"))
(defparameter *loc-folder* (concat *files-folder* "loc/"))
(defparameter *core-text-folder* (concat *files-folder* "core/"))
(defparameter *domain-lists-folder* (concat *files-folder* "domain-lists/"))
(defparameter *boilerplate-folder* (concat *files-folder* "boilerplate/"))
(defparameter *aliases-file* (concat *files-folder* "file-aliases"))
(defparameter *domain-aliases-file* (concat *files-folder* "domain-aliases"))
(defparameter *crawl-data-folder* "../DATA/crawlers/")
(defparameter *discovered-folder* "../DATA/discovered") ;; without terminating slash because of place results (TBD: Inconsistent with other folders)

;; core engine
(defparameter *score-threshold* 1/5)
(defparameter *word-group-size* 400)
(defparameter *boilerplate-threshold* 4)
(defparameter *evidence-length* 6)
(defparameter *smoothing-factor* 1)
(defparameter *allowed-characters* (cl-strings:chars "0123456789 abcdefghijklmnopqrstuvwxyz'"))

;; crawler
(defparameter *crawler-name* "botelaire")
(defparameter *user-agent* "botelaire (https://github.com/xdvom03/klaus, xdvom03 (at) gjk (dot) cz)")
(defparameter *min-word-count* 450)
(defparameter *min-character-comprehensibility* 0.8)
(defparameter *min-word-comprehensibility* 0.6)
(defparameter *forbidden-extensions* (list "css" "png" "mp4" "ico" "svg" "webmanifest" "js" "json" "xml" "jpg" "mp3" "scss" "jsp" "xsl"))
(defparameter *timeout* 20)
(defparameter *link-cap* 40)

;; display
(defparameter *entries-per-page* 10)

;;; CONFIG VARIABLE INIT
;;;----------------------------------------------------------------------------------------------
;;; UTILS

(defun pass ())

(defun one-elem? (lst)
  (and (car lst)
       (null (cdr lst))))

(defun append1 (lst elem)
  (append lst (list elem)))

(defun last1 (lst)
  (car (last lst)))

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

(defun convert-to-str (list)
  (concatenate 'string list))

(defun list-hashes (hashtable)
  ;; TBD: Analogical to hashtable-to-assoc?
  (let ((acc nil))
    (maphash #'(lambda (a b) (push (cons a b) acc))
             hashtable)
    acc))

(defun list-keys (hashtable)
  (let ((acc nil))
    (maphash #'(lambda (a b) (declare (ignore b)) (push a acc))
             hashtable)
    acc))

(defun list-values (hashtable)
  (let ((acc nil))
    (maphash #'(lambda (a b) (declare (ignore a)) (push b acc))
             hashtable)
    acc))

(defun my-round (num &optional (decimals 3))
  (let ((divisor (expt 10 (- decimals))))
    (coerce (* (round num divisor)
               divisor)
            'single-float)))

(defun ln-add (num)
  "Computes ln(1+exp(num)) for ln formulation purposes."
  (if (> num 80) ; avoid floating point overflow (TBD: use constant?)
      num
      (ln (1+ (exp num)))))

(defun ln-add2 (a b)
  (+ a (ln-add (- b a))))

(defun ln+ (&rest numbers)
  "Adds all numbers in the logarithmic formulation. For two numbers, a ln+ b = ln(exp(a)+exp(b))."
  ;; TBD: Tail-optimize
  (if (one-elem? numbers)
      (car numbers)
      (apply #'ln+ (cons (let ((a (first numbers))
                               (b (second numbers)))
                           (+ a (ln-add (- b a))))
                         (cdr (cdr numbers))))))

(defun ln (a)
  ;; TBD: Check efficiency and whether double is really needed
  (log (coerce a 'double-float)))

(defun map-to-hash (fun list &key key-fun)
  ;; key-fun is applied to the list to produce keys.
  (let ((acc (make-hash-table :test #'equal)))
    (dolist (elem list)
      (setf (gethash (if key-fun
                         (funcall key-fun elem)
                         elem)
                     acc)
            (funcall fun elem)))
    acc))

(defun compose (&rest functions)
  ;; result acts right first, as one would naturally write the functions
  (if (one-elem? functions)
      (car functions)
      (lambda (param)
        (funcall (car functions)
                 (funcall (apply #'compose (cdr functions))
                          param)))))

(defun assoc-to-hashtable (list)
  ;; Converts from the assoc list format to the hash table format
  (map-to-hash #'cdr list :key-fun #'car))

(defun hashtable-to-assoc (hashtable)
  ;; Converts from the hash table format to the assoc list format
  (let ((corpus nil))
    (dolist (word (list-keys hashtable))
      (push (cons word (gethash word hashtable)) corpus))
    corpus))

;; only used in debugging crawler results
(defun open-url (url)
  "Open the URL in the browser. Waits a bit for it to load."
  (uiop:run-program (format nil "xdg-open ~S" url))
  (sleep 1))

(defun html (url)
  "Gets HTML data from a URL, but calls an error if it doesn't contain useable text. Error should be resolved within GUI."
  (trivial-timeout:with-timeout (*timeout*)
    ;; connection-timeout does not catch server-side timeouts
    (multiple-value-bind (response status-code headers actual-uri)
        (drakma:http-request url :user-agent *user-agent*)
      ;; There is some additional data returned, but it is safe to ignore
      (let ((content-type (gethash :content-type (assoc-to-hashtable headers))))
        (assert (or (search "text/html" content-type :test #'equal)
                    (search "text/plain" content-type :test #'equal)
                    (search "text/vtt" content-type :test #'equal))
                ;; TBD: Check other text types, maybe just rejecting XML & RSS is okay
                nil
                (concat "No HTML text found. Content type: " content-type))
        (assert (>= 299 status-code 200)
                nil
                (concat "Unsuccessful request. HTTP status code: " status-code))
        (assert (stringp response)
                nil
                (concat "Content not a string. Type: " content-type))
        (values response
                (with-output-to-string (str)
                  ;; The PURI library only lets you print an URI to a stream, not just return it. (which is why we use quri instead!)
                  (puri:render-uri actual-uri str)))))))



(defmacro letrec (bindings &body decls/forms)
  ;; beware that this macro by its very nature cannot show unused variables
  (assert (and (listp bindings)
               (every #'(lambda (b)
                          (or (symbolp b)
                              (and (consp b)
                                   (symbolp (first b))
                                   (null (cddr b)))))
                      bindings))
          (bindings) "malformed bindings")
  (let* ((names (mapcar #'(lambda (b)
                            (etypecase b
                              (symbol b)
                              (cons (first b))))
                        bindings))
         (values (mapcar #'(lambda (b)
                             (etypecase b
                               (symbol nil)
                               (cons (second b))))
                         bindings))
         (nvpairs (reduce #'append (mapcar #'list names values))))
    `(let ,names
       (setf ,@nvpairs)
       (locally
           ,@decls/forms))))

(defmacro fallback (obj if-nil)
  "Identity unless obj is NIL. In that case, returns if-nil."
  ;; avoiding multiple evaluation
  (let ((name (gensym)))
    `(let ((,name ,obj))
       (if ,name
           ,name
           ,if-nil))))
