(defun empty-widget (r c master)
  ;; Used to re-shrink an area after destroying a widget
  (let ((w (widget r c 'ltk:canvas master)))
    (ltk:configure w :width 1)
    (ltk:configure w :height 1)
    w))

(defun warning-box (text title)
  (ltk:message-box text title "ok" "warning"))

(defun info-box (text title)
  (ltk:message-box text title "ok" "info"))

(defun choice-box (text title)
  (equal :yes (ltk:message-box text title "yesno" "question")))

(defun widget (r c type master)
  (let ((w (make-instance type :master master)))
    (ltk:grid w r c :sticky "nesw")
    w))

(defun button (r c master txt command)
  ;; Prevents double clicking while the command is being executed
  (let ((b (widget r c 'ltk:button master)))
    (setf (ltk:text b) txt)
    (setf (ltk:command b) (let ((on? nil))
                            (lambda ()
                              (if (not on?)
                                  (progn
                                    (setf on? t)
                                    (funcall command)
                                    (ltk:after 10 #'(lambda () (setf on? nil))))))))
    b))

(defun button-column (window column page-length &optional (starting-row 0))
  (let ((acc nil))
    (dotimes (i page-length)
      (push (button (+ i starting-row) column window "" #'pass) acc))
    (reverse acc)))

(defun text (r c master txt height width &optional (font "NotoSans 10"))
  (let ((tex (widget r c 'ltk:text master)))
    (ltk:configure tex :height height)
    (ltk:configure tex :width width)
    (ltk:configure tex :font font)
    (setf (ltk:text tex) txt)
    tex))

(defun read-text-widget (text-widget)
  ;; For whatever reason, reading from a text always adds a newline
  (let ((str (ltk:text text-widget)))
    (subseq str 0 (1- (length str)))))

(defun listbox (r c master txt)
  (let ((l (widget r c 'ltk:listbox master)))
    (setf (ltk:text l) txt)
    l))

(defun entry (r c master &optional (txt ""))
  (let ((e (widget r c 'ltk:entry master)))
    (setf (ltk:text e) txt)
    e))

(defun checkbox (r c master text command)
  (let ((ch (widget r c 'ltk:check-button master)))
    (setf (ltk:command ch) #'(lambda (a)
                               (declare (ignore a))
                               ;(setf (ltk:value ch))
                               (funcall command)))
    (setf (ltk:text ch) text)
    ch))

(defun label (r c master txt)
  (let ((l (widget r c 'ltk:label master)))
    (setf (ltk:text l) txt)
    (ltk:configure l :anchor :center)
    l))

(defun frame (r c master)
  ;; Ugly hack: LTK does not support backround colours of frames, but it works for canvases, and they seem to work serviceably as frames. 
  (let* ((f (widget r c 'ltk:canvas master)))
    ;; This frame does nothing (and gets overwritten), but frames don't collapse when all their widgets are destroyed, so we want something invisible to always remain
    (widget 0 0 'ltk:frame f)
    (ltk:configure f :width 0)
    (ltk:configure f :height 0)
    (ltk:configure f :background *frame-colour*)
    f))

(defun window (title)
  (let ((W (make-instance 'ltk:toplevel :title title)))
    (ltk:set-geometry-xy W 0 0)
    W))

(defun progress-bar (r c master &optional (bar-col "#692") (bg-col "#841"))
  (let ((pb (widget r c 'ltk-mw:progress master)))
    (setf (ltk-mw:bar-color pb) bar-col)
    (ltk:configure pb :background bg-col)
    pb))

(defun scrollable-list (r c master page-length lst &optional function-lst)
  ;; No function list will assume no button functions.
  ;; Returns the frame within which it exists
  (let* ((acc (frame r c master))
         (start 0)
         (f (frame (length lst) 0 acc))
         (l (label (1+ (length lst)) 0 acc ""))
         (buttonlist (button-column acc 0 page-length))
         left
         right
         (redraw #'(lambda ()
                     (dotimes (i page-length)
                       (let ((b (nth i buttonlist)))
                         ;; TBD: This still looks ugly
                         (if (> (length lst) (+ start i) -1)
                             (progn
                               (ltk:grid b i 0 :sticky "nesw")
                               (setf (ltk:text b) (nth (+ start i) lst))
                               (setf (ltk:command b) (if function-lst
                                                         (nth (+ start i) function-lst))))
                             (progn
                               (ltk:grid-forget b)
                               (setf (ltk:text b) "")
                               (setf (ltk:command b) #'pass)))))
                     (setf (ltk:text l) (concat start "/" (length lst)))
                     (if (>= start page-length)
                         (ltk:configure left :state :normal)
                         (ltk:configure left :state :disabled))
                     (if (< start (- (length lst) page-length))
                         (ltk:configure right :state :normal)
                         (ltk:configure right :state :disabled)))))
    (setf left (button 0 0 f "<-" #'(lambda ()
                                     (decf start page-length)
                                     (funcall redraw))))
    (setf right (button 0 1 f "->" #'(lambda ()
                                      (incf start page-length)
                                      (funcall redraw))))
    (funcall redraw)
    acc))

(defun show-time (timer message)
  ;; gets the time of start
  (info-box (concat message " Time taken: " (my-round (/ (- (get-internal-real-time) timer) internal-time-units-per-second))) "success!"))

(defun destroy-widgets (list)
  (dolist (w list)
    (ltk:destroy w)))

(defun comment (r c master)
  (text r c master "" 15 35))

(defmacro warn-on-error ((error-title) &body body)
  (let ((title-var (gensym)))
    `(let ((,title-var ,error-title))
       (handler-case (progn
                       ,@body)
         (error (err-text)
           (warning-box err-text ,title-var)
           (abort))))))

;;; VARIOUS UTILS
;;;----------------------------------------------------------------------------------------------
;;; GENERAL BUCKET SYSTEM

(defun next-mode (mode modes-cycle)
  (fallback (second (member mode modes-cycle :test #'equal))
            (first modes-cycle)))

(defun bucket-section (r c master button-action refresher modes-cycle data-fun)
  ;; data-fun is called as (data-fun url origin) to produce button labels
  (let* ((f (frame r c master))
         (fr (frame 0 0 f))
         bucket-buttons
         bucket-data
         (mode (first modes-cycle)))
                  
    (letrec ((b (button 0 1 f mode #'(lambda () (setf (ltk:text b) (setf mode (next-mode mode modes-cycle)))))))
      (labels ((new-bucket-button (url origin)
                 (let ((button-text (funcall data-fun url origin)))
                   (if (find button-text bucket-data :test #'equal)
                       (warning-box "Already in bucket." "Nope!")
                       (letrec ((index (length bucket-buttons))
                                (new-button (button index
                                                    0
                                                    fr
                                                    button-text
                                                    #'(lambda ()
                                                        (funcall button-action url origin (get-current-class) mode)
                                                        (setf bucket-data (remove button-text bucket-data :test #'equal))
                                                        (ltk:destroy new-button)
                                                        (funcall refresher)))))
                         (push button-text bucket-data)
                         (push new-button bucket-buttons))))))
        #'new-bucket-button))))

;;; GENERAL BUCKET SYSTEM
;;;----------------------------------------------------------------------------------------------
;;; HORRIFYING ZONE

(defun choose-file (&key (initialdir (namestring ltk::*default-pathname-defaults*))
			 parent title mustexist)
  ;; this was a total shot in the dark, but somehow it actually works?
  (ltk:format-wish "senddatastring [tk_getOpenFile ~@[ -initialdir \"~a\"~]~@[ -parent ~a ~]~@[ -title {~a}~]~@[ -mustexist ~a~]]" (ltk::tkescape2 initialdir) (and parent (ltk:widget-path parent)) title (and mustexist 1))
  (ltk::read-data))
