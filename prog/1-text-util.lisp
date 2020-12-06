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

(defun fast-substr-check (text key start)
  (let ((key-len (length key))
        (text-len (length text)))
    (and (< (+ start key-len) text-len)
         (equal (char text start) (char key 0)) ; simple optimisation - look first for a single matching character, eliminating most wrong guesses
         (equal (subseq text start (+ start key-len)) key))))

(defun remove-enclosed (text delim1 delim2)
  ;; TBD: Verify
  "Removes all text enclosed between delim1 and delim2, including the tags. Returns remaining text. Nondestructive."
  (let ((acc nil)
        (enclosed-p nil))
    (dotimes (i (length text))
      (cond ((fast-substr-check text delim1 i)
             (setf enclosed-p t))
            ((fast-substr-check text delim2 i)
             (setf enclosed-p nil)
             (setf i (+ i (length delim2) -1)))
            (t (if (not enclosed-p)
                   (push (char text i) acc)))))
    (convert-to-str (reverse acc))))

(defun filter (text allow-rule censor-by)
  (let ((acc nil))
    (dotimes (i (length text))
      (if (funcall allow-rule (char text i))
          (push (char text i) acc)
          (push censor-by acc)))
    (convert-to-str (reverse acc))))

(defun remove-multiple-spaces (text)
  (let ((acc nil))
    (dotimes (i (length text))
      (let ((char (char text i))
            (previous-char (if (> i 0)
                               (char text (1- i)))))
        (if (not (multi-equal char previous-char (char " " 0)))
            (push char acc))))
    (reverse (convert-to-str acc))))

(defun make-safe (text)
  ;; Only keeps well-known characters to prevent printing trouble
  (filter text #'(lambda (a) (member a '(#\  #\EM_DASH #\LEFTWARDS_ARROW #\EN_DASH #\PLUS-MINUS_SIGN #\THIN_SPACE #\MINUS_SIGN #\' #\DEGREE_SIGN #\# #\6 #\Tab #\_ #\7 #\% #\& #\? #\+ #\@ #\} #\Z #\X #\M #\z #\H #\q #\V #\K #\J #\y
                                         #\x #\B #\* #\G #\A #\9 #\4 #\5 #\3 #\I #\v #\0 #\b #\S #\: #\{ #\f #\] #\[ #\Q #\L #\R #\w #\; #\2 #\1 #\, #\$ #\) #\\ #\| #\^ #\( #\N #\. #\u #\p #\k #\W #\/ #\8 #\F #\U #\r #\d #\g #\j
                                         #\o #\- #\n #\e #\i #\" #\= #\s #\a #\c #\> #\l #\m #\t #\h #\E #\P #\Y #\T #\C #\O #\D #\! #\< #\  #\Newline)))
          (char "*" 0)))

(defun remove-punctuation (text)
  ;; Replaces commas, parentheses, periods, etc. by spaces
  ;; Also replaces whitespace
  ;; TBD: Remove double and multiple spaces
  (filter text #'(lambda (a)
                   (not (member a '(#\EM_DASH #\LEFTWARDS_ARROW #\EN_DASH #\PLUS-MINUS_SIGN #\THIN_SPACE #\MINUS_SIGN #\DEGREE_SIGN #\# #\& #\+ #\}
                                    #\* #\: #\{ #\] #\[ #\; #\, #\) #\\ #\| #\^ #\( #\. #\/ #\-
                                    #\" #\= #\> #\< #\Newline #\Tab))))
          (char " " 0)))

(defun remove-diacritics (str)
  (map 'string
       #'basic-letter
       str))

(defun basic-letter (ltr)
  (case ltr
    (#\á #\a)
    (#\é #\e)
    (#\ě #\e)
    (#\í #\i)
    (#\ó #\o)
    (#\ú #\u)
    (#\ů #\u)
    (#\ý #\y)
    (#\č #\c)
    (#\ď #\d)
    (#\ň #\n)
    (#\ř #\r)
    (#\š #\s)
    (#\ť #\t)
    (#\ž #\z)
    (#\RIGHT_SINGLE_QUOTATION_MARK #\')
    (otherwise ltr)))

(defun tokens (text)
  ;; TEMP: Really imperative and possibly misplaced
  (let* ((?s 0)
         (!s 0)
         (words (remove-if #'(lambda (word) (equal word ""))
                           (split text #\ )))
         (normal-words (mapcar #'(lambda (word)
                                   (if (equal (last-char word) #\!)
                                       (progn
                                         (incf !s)
                                         (cut-word word))
                                       (if (equal (last-char word) #\?)
                                           (progn
                                             (incf ?s)
                                             (cut-word word))
                                           word)))
                               words)))
    ;; TEMP: Spammed words are the worst
    (remove-duplicates (append normal-words
                               (make-list ?s :initial-element "?")
                               (make-list !s :initial-element "!"))
                       :test #'equal)))

(defun last-char (str)
  (char str (1- (length str))))

(defun cut-word (word)
  (subseq word 0 (1- (length word))))
