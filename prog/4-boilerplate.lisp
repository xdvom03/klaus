;; detecting repeated content

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
  ;; TBD: Why does this not work with words as symbols?
  (let ((urls (remove-if-not #'(lambda (url)
                                 (equal (find-domain url)
                                        domain))
                             (class-urls "/" t))))
    (if (cdr urls)
        (reduce #'multi-overlap
                (append (list (list (cl-strings:split (read-text (car urls))))) ;; remember: one level of append gets lost!
                        (mapcar #'(lambda (url) (cl-strings:split (read-text url)))
                                (cdr urls)))))))

(defun remove-substr (text substr)
  ;; TEMP: Obviously not optimal, should not happen! Probably an underlying bug.
  (fallback (ignore-errors
             (let ((index (search substr text :test #'equal)))
               (concat (subseq text 0 index) (subseq text (+ index (length substr))))))
            text))
