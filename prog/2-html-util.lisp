(defun open-url (url)
  "Open the URL in the browser. Waits a bit for it to load."
  (uiop:run-program (format nil "xdg-open ~S" url))
  (sleep 1))

(defun find-links (text)
  ;; ignores <a> tags, since the only relevant part of them is the href. 
  (append (find-enclosed-text text "href=\"" "\"")
          (find-enclosed-text text "href='" "'")))

(defun safe-fetch-html (url)
  "Gets HTML data from a URL, but if it 404's, it returns nothing found."
  ;; TBD: Figure out how to dynamically insert the bot name into the headers
  (let ((unsafe (ignore-errors (trivial-timeout:with-timeout (*timeout*)
                                 ;; BUG: Errors if the site contains any invalid UTF-8 characters.
                                 (drakma:http-request url :user-agent "botelaire (https://github.com/xdvom03/klaus, xdvom03 (at) gjk (dot) cz)")))))
    ;; an image link, or anything that isn't a string, is considered a 404
    (if (or (null unsafe) (not (stringp unsafe)))
        "nothingfound"
        unsafe)))

(defun url-links (url)
  "Returns a list of all links found in url"
  (find-links (safe-fetch-html url)))

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
  ;; TBD: Deal properly with various forms of relative links (fails with Tailsteak site). For now, keep old slash version.
  ;; Anything not starting with http is assumed to be a relative link, starting slash removed, domain name appended
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
                                                         (url-links url))))
                       :test #'equal)))

(defun find-domain (url)
  (do ((i 0 (1+ i))
       (slashes 2 (- slashes (if (slash? (char url i)) 1 0))))
      ((or (< slashes 0)
           (>= i (length url)))
       (subseq url 0 (if (< slashes 0) ; if found a third slash, cut it, elsewhere keep the whole domain
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
  (remove-enclosed text "<" ">" #\ ))

(defun remove-fluff (text)
  (remove-enclosed (remove-enclosed text
                                    "<style" "</style>")
                   "<script" "</script>" #\ ))

(defun raw-text (url)
  (remove-multiple-spaces (remove-punctuation (remove-diacritics (decode-xml-entities (remove-tags (remove-fluff (string-downcase (coerce (safe-fetch-html url) 'simple-string)))))))))

(defun clean-text (txt)
  (remove-multiple-spaces (make-safe txt)))

(defun url-text (url)
  ;; Fetches text of a url
  ;; Placeholder for now until the MAJOR REFACTORING
  ;; Duplicate-ish of extract-text
  (let* ((raw (safe-fetch-html url))
         ;; TEMP: For some reason, not simple-string
         (safe (string-downcase (decode-xml-entities (coerce raw 'simple-string))))
         (content (remove-multiple-spaces (remove-punctuation (remove-diacritics (make-safe (remove-tags (remove-fluff safe))))))))
    content))

(defun extract-text (html)
  (let* ((safe (make-safe (remove-diacritics (string-downcase html))))
         (content (remove-punctuation (remove-tags (remove-enclosed (remove-enclosed safe "<style" "</style>") "<script" "</script>")))))
    content))
