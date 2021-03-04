(ql:quickload (list "ltk" "drakma" "quri" "plump" "trivial-timeout"))

;;; IMPORTS
;;;----------------------------------------------------------------------------------------------
;;; UTILS

(defun charlist (str)
  (map 'list #'identity str))

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

(defun replace-last (lst elem)
  ;; If list is NIL, returns NIL
  (if (null lst)
      nil
      (if (null (cdr lst))
          (list elem)
          (cons (car lst)
                (replace-last (cdr lst) elem)))))

(defun slash? (char)
  (equal char (char "/" 0)))

(defun folder? (path)
  (slash? (char (namestring path) (1- (length (namestring path))))))

(defun convert-to-str (list)
  (concatenate 'string list))

(defun list-hashes (hashtable) ;; TBD: Check usage
  (let ((acc nil))
    (maphash #'(lambda (a b) (push (cons a b) acc))
             hashtable)
    acc))

(defun list-keys (hashtable)
  (let ((acc nil))
    ;; Stop the warning by adding a useless b
    (maphash #'(lambda (a b) b (push a acc))
             hashtable)
    acc))

(defun multi-equal (&rest things)
  (if (cdr things)
      (if (equal (car things)
                 (second things))
          (apply #'multi-equal (cdr things)))
      (car things)))

(defun my-round (num &optional (decimals 3))
  (let ((divisor (expt 10 (- decimals))))
    (coerce (* (round num divisor)
               divisor)
            'single-float)))

(defun ln-add (num)
  "Computes ln(1+exp(num)) for ln formulation purposes."
  (if (> num 80)
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
  ;; Ln formulation requires high float precision
  ;; TBD: Does it still?
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

(defun cut (lst size)
  (let ((acc nil)
        (acc2 nil))
    (dotimes (i (length lst))
      (push (nth i lst) acc2)
      (if (zerop (mod (1+ i) size))
          (progn
            (push (reverse acc2) acc)
            (setf acc2 nil))))
    (append1 (reverse acc) (reverse acc2))))

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

(defun join (fragments filler)
  ;; not tail-recursive
  (if (cdr fragments)
      (join (cons (concat (first fragments) filler (second fragments)) (cddr fragments)) filler)
      (car fragments)))

(defun assoc-to-hashtable (list)
  ;; Converts from the assoc list format to the hash table format
  (map-to-hash #'cdr list :key-fun #'car))

(defun hashtable-to-assoc (hashtable)
  ;; Converts from the hash table format to the assoc list format
  (let ((corpus nil))
    (dolist (word (list-keys hashtable))
      (push (cons word (gethash word hashtable)) corpus))
    corpus))

(defun open-url (url)
  "Open the URL in the browser. Waits a bit for it to load."
  (uiop:run-program (format nil "xdg-open ~S" url))
  (sleep 1))



(defmacro letrec (bindings &body decls/forms)
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
           (progn ;; TBD: Is this needed?
             ,if-nil)))))

;;; UTILS
;;;----------------------------------------------------------------------------------------------
;;; CONFIG VARIABLES

;; paths
(defparameter *classes-folder* "../DATA/classes/")
(defparameter *generated-data-folder* "../DATA/generated-data/")
(defparameter *files-folder* "../DATA/files/")
(defparameter *html-folder* (concat *files-folder* "html/"))
(defparameter *text-folder* (concat *files-folder* "text/"))
(defparameter *core-text-folder* (concat *files-folder* "core/"))
(defparameter *domain-lists-folder* (concat *files-folder* "domain-lists/"))
(defparameter *boilerplate-folder* (concat *files-folder* "boilerplate/"))
(defparameter *aliases-file* (concat *files-folder* "file-aliases"))
(defparameter *domain-aliases-file* (concat *files-folder* "domain-aliases"))
(defparameter *crawl-data-folder* "../DATA/crawlers/")
(defparameter *discovered-folder* "../DATA/discovered") ;; without terminating slash because of place results

;; core engine
(defparameter *score-threshold* 1/5)
(defparameter *word-group-size* 1000)
(defparameter *boilerplate-threshold* 4)
(defparameter *evidence-length* 6)
(defparameter *smoothing-factor* 1)
(defparameter *allowed-characters* (charlist "0123456789 ,.abcdefghijklmnopqrstuvwxyz"))

;; crawler
(defparameter *crawler-name* "botelaire")
(defparameter *user-agent* "botelaire (https://github.com/xdvom03/klaus, xdvom03 (at) gjk (dot) cz)")
(defparameter *min-word-count* 450)
(defparameter *min-character-comprehensibility* 0.8)
(defparameter *min-word-comprehensibility* 0.6)
(defparameter *forbidden-extensions* (list "css" "png" "mp4" "ico" "svg" "webmanifest" "js" "json" "xml" "jpg" "mp3" "scss" "jsp" "xsl"))
(defparameter *timeout* 10)

;; display
(defparameter *entries-per-page* 10)
(defparameter *bg-col* "#f0f0f0")
(defparameter *button-col* "#e0e0e0")
(defparameter *active-col* "#a0a0a0")
(defparameter *text-col* "#000000")

;; TBD: Should not be global!
(defparameter *explain?* nil)
(defparameter *blind?* nil)
