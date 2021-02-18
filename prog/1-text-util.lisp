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

(defun charlist (str)
  (map 'list #'identity str))

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
                           (append (reverse (charlist swap-for)) acc)))))
            ((fast-substr-check text delim2 i)
             (setf enclosed-p nil)
             (setf i (+ i (length delim2) -1))
             (push (char text i) acc))
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
                                         #\o #\- #\n #\e #\i #\" #\= #\s #\a #\c #\> #\l #\m #\t #\h #\E #\P #\Y #\T #\C #\O #\D #\! #\< #\  #\Newline)))))

(defun remove-punctuation (text)
  ;; Replaces commas, parentheses, periods, etc. by spaces
  ;; Also replaces whitespace
  ;; TBD: Remove double and multiple spaces
  (filter text #'(lambda (a)
                   (not (member a '(#\EM_DASH #\LEFTWARDS_ARROW #\EN_DASH #\PLUS-MINUS_SIGN #\THIN_SPACE #\MINUS_SIGN #\DEGREE_SIGN #\# #\& #\+ #\}
                                    #\* #\: #\{ #\] #\[ #\; #\) #\\ #\| #\^ #\( #\/ #\- #\. #\, #\? #\!
                                    #\" #\= #\> #\< #\Newline #\Tab #\Return))))
          (char " " 0)))

(defun remove-diacritics (str)
  (map 'string
       #'basic-letter
       str))

(defun basic-letter (ltr)
  (if (eq ltr #\RIGHT_SINGLE_QUOTATION_MARK)
      #\'
      ltr))

(defun wordlist (text)
  (mapcar #'intern (remove-if #'(lambda (word) (equal word ""))
                              (split text #\ ))))

(defun tokens (text)
  (let* ((raw (wordlist text))
         (corp (make-hash-table :size (length raw) :test #'eq)) ;; TEMP: Trying to speed up
         (acc (make-array *word-group-size* :initial-element nil))
         (counter 0)
         (weight 1) ;; TEMP: this will become a folder weight
         )
    (dolist (word raw)
      (if (not (some #'(lambda (a) (eq a word)) acc)) ;; This takes the majority of corpus rebuilding time
          (if (gethash word corp)
              (incf (gethash word corp) weight)
              (setf (gethash word corp) weight)))
      (setf (aref acc (mod counter *word-group-size*)) word)
      (incf counter))
    corp))

(defun last-char (str)
  (char str (1- (length str))))

(defun cut-word (word)
  (subseq word 0 (1- (length word))))

(defun decode-xml-entities (txt)
  ;; plump may cause "junk in string". Cause unknown, happens rarely, so the decoding is skipped if the error occurs.
  (fallback (ignore-errors (plump:decode-entities txt))
            txt))





(defun overlap-length (txt1 txt2)
  ;; only looks for sequences beginning at 0
  (let ((max-len (min (length txt1)
                      (length txt2))))
    (do ((len 0 (1+ len)))
        ((or (equal len max-len)
             (not (search (subseq txt1 0 (1+ len)) txt2 :test #'equal)))
         len))))

(defun overlap (txt1 txt2)
  (let ((len (min (length txt1) (length txt2)))
        (acc nil))
    (do ((pos 0))
        ((>= pos (- len *boilerplate-threshold*)))
      (if (search (subseq txt1 pos (+ pos *boilerplate-threshold*))
                  txt2
                  :test #'equal)
          (let ((overlap (overlap-length (subseq txt1 pos) txt2)))
            (push (subseq txt1 pos (+ pos overlap)) acc)
            (incf pos overlap))
          (incf pos)))
    (reverse acc)))

(defun multi-overlap (texts txt2)
  (apply #'append
         (mapcar #'(lambda (txt1)
                     (overlap txt1 txt2))
                 texts)))

(defun boilerplate (domain)
  (let ((links (remove-if-not #'(lambda (link)
                                  (equal (find-domain link)
                                         domain))
                              (mapcar #'car (aliases)))))
    (if (cdr links)
        (reduce #'multi-overlap
                (append (list (list (split (read-text (car links)) #\ ))) ;; remember: one level of append gets lost!
                        (mapcar #'(lambda (url) (split (read-text url) #\ ))
                                (cdr links)))))))

(defun remove-substr (text substr)
  ;; TEMP: Obviously not optimal, should not happen! Probably an underlying bug.
  (fallback (ignore-errors
             (let ((index (search substr text :test #'equal)))
               (concat (subseq text 0 index) (subseq text (+ index (length substr))))))
            text))

(defun core-text (url)
  (let* ((text (url-text url))
         (boilerplate (join (boilerplate (find-domain url)) " "))
         (mustard (search boilerplate text :test #'equal)))
    (concat (subseq text 0 mustard) (subseq text (+ mustard (length boilerplate))))))

(defun core-text-known (url)
  (let* ((text (read-text url))
         (boilerplate (mapcar #'(lambda (words) (join words " "))
                              (boilerplate (find-domain url)))))
    (reduce #'remove-substr (append (list text)
                                    boilerplate))))
