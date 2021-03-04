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
  ;; First checks whether the first character matches, eliminating most incorrect guesses
  (let ((key-len (length key))
        (text-len (length text)))
    (and (< (+ start key-len) text-len)
         (equal (char text start) (char key 0))
         (equal (subseq text start (+ start key-len)) key))))

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

(defun remove-multiple-spaces (text)
  (let ((acc nil))
    (dotimes (i (length text))
      (let ((char (char text i))
            (previous-char (if (> i 0)
                               (char text (1- i)))))
        (if (not (multi-equal char previous-char (char " " 0)))
            (push char acc))))
    (reverse (convert-to-str acc))))

(defun remove-diacritics (str)
  (map 'string
       #'basic-letter
       str))

(defun basic-letter (ltr)
  (case ltr
    ((#\RIGHT_SINGLE_QUOTATION_MARK #\')
     (#\á #\a)))
  (if (eq ltr #\RIGHT_SINGLE_QUOTATION_MARK)
      #\'
      ltr))

(defun make-safe (text)
  ;; Only keeps well-known characters
  (filter text #'(lambda (a) (member a *allowed-characters*))))

(defun wordlist (text)
  (mapcar #'intern (split text #\ )))

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

(defun decode-xml-entities (txt)
  ;; plump may cause "junk in string". Cause unknown, happens rarely, so the decoding is skipped if the error occurs.
  (fallback (ignore-errors (plump:decode-entities txt))
            txt))
