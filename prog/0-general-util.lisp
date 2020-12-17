;; TBD: Clean and divide

(ql:quickload (list "ltk" "dexador" "quri" "plump" "trivial-timeout"))

;;; IMPORTS
;;;----------------------------------------------------------------------------------------------
;;; UTILS

(defparameter *classes-folder* "../DATA/classes/")
(defparameter *files-folder* "../DATA/files/")
(defparameter *aliases-file* "../DATA/file-aliases")
;; BEWARE: Do not change without knowing what you are doing! Can mess up history!
;; TBD: Turn this into another folder
(defparameter *history-file* "../DATA/history/history")
(defparameter *history-temp-file* "../DATA/history/history2")
(defparameter *history-rename* "history")

(defparameter *wanderbot-file* "../DATA/klaus/wanderbot")

(defparameter *entries-per-page* 10)
(defparameter *try-to-class?* t)
(defparameter *explain?* nil)
(defparameter *evidence-length* 6)
(defparameter *newline* "
")
(defparameter *iterations* 200)
(defparameter *decimals* 3)
(defparameter *smoothing-factor* 1)
(defparameter *crawler-name* "botelaire")

(defparameter *min-word-score* 1/5)
(defparameter *max-word-score* 4/5)

(defparameter *forbidden-extensions* (list "css" "png" "mp4" "ico" "svg" "webmanifest" "js" "json" "xml"))
(defparameter *timeout* 5)

(defparameter *bg-col* "#f0f0f0")
(defparameter *button-col* "#e0e0e0")
(defparameter *active-col* "#a0a0a0")
(defparameter *text-col* "#000000")

(defun pass ())

(defmacro fallback (obj if-nil)
  "Identity unless obj is NIL. In that case, returns if-nil."
  ;; avoiding multiple evaluation
  (let ((name (gensym)))
    `(let ((,name ,obj))
       (if ,name
           ,name
           (progn
             ,if-nil)))))

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

(defun my-round (num &optional (decimals *decimals*))
  (let ((divisor (expt 10 (- decimals))))
    (coerce (* (round num divisor)
               divisor)
            'double-float)))

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
