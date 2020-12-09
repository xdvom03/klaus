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
  (let ((unsafe (ignore-errors (dex:get url :headers '(("User-Agent" . "Botelaire, crawler for https://github.com/xdvom03/klaus (reads robots.txt for botelaire). In case of any trouble, contact xdvom03 [at] gjk [dot] cz."))
                                        :timeout 2))))
    ;; an image link, or anything that isn't a string, is considered a 404
    (if (or (null unsafe) (not (stringp unsafe)))
        "nothingfound"
        unsafe)))

(defun url-links (url)
  "Returns a list of all links found in url"
  (find-links (safe-fetch-html url)))

(defun extract-link (link domain)
  ;; Empty links happen sometimes, as do super short ones, should be removed
  ;; TBD: Deal properly with various forms of relative links (fails with Tailsteak site). For now, keep old slash version.
  ;; Anything not starting with http is assumed to be a relative link, starting slash removed, domain name appended
  (if (and (> (length link) 0)
           (slash? (char link 0)))
      (concat domain
              link)
      link))

(defun vetted-links (url)
  (let ((domain (find-domain url)))
    (remove-if-not #'(lambda (str) (and (> (length str) 0)
                                        (equal (char str 0) (char "h" 0))))
                   (mapcar #'(lambda (tag) (extract-link tag domain)) (url-links url)))))

(defun find-domain (url)
  (do ((i 0 (1+ i))
       (slashes 2 (- slashes (if (slash? (char url i)) 1 0))))
      ((or (< slashes 0)
           (>= i (length url)))
       (if (slash? (char url (1- i)))
           (subseq url 0 (1- i))
           (subseq url 0 i)))))

(defun remove-domain (url)
  (do ((i 0 (1+ i))
       (slashes 2 (- slashes (if (slash? (char url i)) 1 0))))
      ((or (< slashes 0)
           (>= i (length url)))
       (if (slash? (char url (1- i)))
           (subseq url (1- i))
           (subseq url i)))))

(defun remove-tags (text)
  (remove-enclosed text "<" ">"))

(defun url-text (url)
  ;; Fetches text of an url
  ;; Placeholder for now until the MAJOR REFACTORING
  ;; Duplicate-ish of extract-text
  (let* ((raw (safe-fetch-html url))
         (safe (make-safe (remove-diacritics (string-downcase (plump:decode-entities raw)))))
         (content (remove-multiple-spaces (remove-punctuation (remove-tags (remove-enclosed (remove-enclosed safe "<style" "</style>") "<script" "</script>"))))))
    content))

(defun extract-text (html)
  (let* ((safe (make-safe (remove-diacritics (string-downcase html))))
         (content (remove-punctuation (remove-tags (remove-enclosed (remove-enclosed safe "<style" "</style>") "<script" "</script>")))))
    content))
