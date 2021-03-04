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
  (let ((links (remove-if-not #'(lambda (link)
                                  (equal (find-domain link)
                                         domain))
                              (mapcar #'car (aliases)))))
    (if (cdr links)
        (reduce #'multi-overlap
                (append (list (list (cl-strings:split (read-text (car links))))) ;; remember: one level of append gets lost!
                        (mapcar #'(lambda (url) (cl-strings:split (read-text url)))
                                (cdr links)))))))

(defun remove-substr (text substr)
  ;; TEMP: Obviously not optimal, should not happen! Probably an underlying bug.
  (fallback (ignore-errors
             (let ((index (search substr text :test #'equal)))
               (concat (subseq text 0 index) (subseq text (+ index (length substr))))))
            text))

(defun core-text (url)
  (let* ((text (url-text url))
         (boilerplate (cl-strings:join (boilerplate (find-domain url)) :separator " "))
         (mustard (search boilerplate text :test #'equal)))
    (concat (subseq text 0 mustard) (subseq text (+ mustard (length boilerplate))))))

(defun core-text-known (url)
  (let* ((text (read-text url))
         (boilerplate (mapcar #'(lambda (words) (cl-strings:join words :separator " "))
                              (boilerplate (find-domain url)))))
    (reduce #'remove-substr (append (list text)
                                    boilerplate))))
