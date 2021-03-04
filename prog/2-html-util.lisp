(defun find-links (text)
  ;; only looks for href
  ;; does not consider tags (could theoretically find literal text, but that seems like a non-issue)
  (append (find-enclosed-text text "href=\"" "\"")
          (find-enclosed-text text "href='" "'")))

(defun safe-fetch-html (url)
  "Gets HTML data from a URL, but if it 404's, it returns nothing found."
  (let ((unsafe (ignore-errors (trivial-timeout:with-timeout (*timeout*)
                                 ;; BUG: Errors if the site contains any invalid UTF-8 characters.
                                 (drakma:http-request url :user-agent *user-agent*)))))
    ;; an image link, or anything that isn't a string, is considered a 404
    (if (or (null unsafe) (not (stringp unsafe)))
        "nothingfound"
        (coerce unsafe 'simple-string))))

(defun absolute-link? (link)
  (and (> (length link) 3)
       (equal (subseq link 0 4) "http")))

(defun relative-protocol-link? (link)
  (and (> (length link) 1)
       (equal (subseq link 0 2) "//")))

(defun anchor-link? (link)
  (and (> (length link) 0)
       (equal (char link 0) #\#)))

(defun extract-link (link domain)
  ;; Empty links happen sometimes, as do super short ones, should be removed
  ;; Anything not starting with http is assumed to be a relative link, starting slash removed, domain name prepended
  (if (absolute-link? link)
      link
      ;; double slash is relative protocol
      (if (relative-protocol-link? link)
          (concat "https:"
                  link)
          (concat domain
                  link))))

(defun vetted-links (url)
  (let ((domain (find-domain url)))
    (remove-duplicates (remove-if-not #'absolute-link?
                                      (mapcar #'(lambda (link) (extract-link link domain))
                                              (remove-if #'anchor-link?
                                                         (find-links (safe-fetch-html url)))))
                       :test #'equal)))

(defun find-domain (url)
  (do ((i 0 (1+ i))
       (slashes 2 (- slashes (if (slash? (char url i)) 1 0))))
      ((or (< slashes 0)
           (>= i (length url)))
       (subseq url 0 (if (< slashes 0) ; if found a third slash, cut it, but if it reached the end, keep the whole domain
                         (1- i)
                         i)))))

(defun remove-domain (url)
  (do ((i 0 (1+ i))
       (slashes 2 (- slashes (if (slash? (char url i)) 1 0))))
      ((or (< slashes 0)
           (>= i (length url)))
       (if (slash? (char url (1- i)))
           (subseq url (1- i))
           (subseq url i)))))

(defun remove-tags (text)
  ;; (remove-enclosed text "<img" ">" " ~image~ ")
  (remove-enclosed text "<" ">" #\ ))

(defun remove-fluff (text)
  (remove-tags (remove-enclosed (remove-enclosed text
                                                 "<style" "</style>" #\ )
                                "<script" "</script>" #\ )))

(defun raw-text (url)
  (extract-raw-text (safe-fetch-html url)))

(defun clean-text (txt)
  (remove-multiple-spaces (make-safe txt)))

(defun url-text (url)
  ;; Fetches text of a url
  (extract-text (safe-fetch-html url)))

(defun extract-text (html)
  (remove-multiple-spaces (make-safe (remove-punctuation (remove-diacritics (decode-xml-entities (remove-fluff (string-downcase html))))))))

(defun extract-raw-text (html)
  (remove-multiple-spaces (remove-punctuation (remove-diacritics (decode-xml-entities (remove-fluff (string-downcase html)))))))


;; TBD: Create a single character fixing function which will replace everything (?) with spaces
