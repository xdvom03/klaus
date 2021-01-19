(defun empty-widget (r c master)
  ;; Used to re-shrink an area after destroying a widget
  (let ((w (widget r c 'ltk:canvas master)))
    (ltk:configure w :width 1)
    (ltk:configure w :height 1)
    w))

(defun warning-box (text title)
  (ltk:message-box text title "ok" "warning"))

(defun widget (r c type master)
  (let ((w (make-instance type :master master)))
    (ltk:grid w r c :sticky "nesw")
    ;; TEMP: Disabled because it messes up the debugger (ltk:after 0 #'(lambda () (ltk:mainloop))) ;; TBD: This is ugly. How exactly does LTK deal with showing things?
    w))

(defun button (r c master txt command)
  (let ((b (widget r c 'ltk:button master)))
    (setf (ltk:text b) txt)
    (setf (ltk:command b) command)
    (ltk:configure b :background *button-col*)
    (ltk:configure b :foreground *text-col*)
    (ltk:configure b :activebackground *active-col*)
    b))

(defun button-column (window column page-length &optional (starting-row 0))
  (let ((acc nil))
    (dotimes (i page-length)
      (push (button (+ i starting-row) column window "" #'pass) acc))
    (reverse acc)))

(defun text (r c master txt height width font)
  (let ((tex (widget r c 'ltk:text master)))
    (ltk:configure tex :height height)
    (ltk:configure tex :width width)
    (ltk:configure tex :font font)
    (setf (ltk:text tex) txt)
    tex))

(defun read-text (text-widget)
  ;; For whatever reason, reading from a text always adds a newline
  (let ((str (ltk:text text-widget)))
    (subseq str 0 (1- (length str)))))

(defun listbox (r c master txt)
  (let ((l (widget r c 'ltk:listbox master)))
    (setf (ltk:text l) txt)
    l))

(defun entry (r c master)
  (widget r c 'ltk:entry master))

(defun checkbox (r c master text command)
  (let ((ch (widget r c 'ltk:check-button master)))
    (setf (ltk:command ch) command)
    (setf (ltk:text ch) text)
    ch))

(defun label (r c master txt)
  (let ((l (widget r c 'ltk:label master)))
    (setf (ltk:text l) txt)
    (ltk:configure l :anchor :center)
    (ltk:configure l :background *bg-col*)
    (ltk:configure l :foreground *text-col*)
    l))

(defun frame (r c master)
  ;; Ugly hack: LTK does not support backround colours of frames, but it works for canvases, and they seem to work serviceably as frames. 
  (let ((f (widget r c 'ltk:canvas master)))
    (ltk:configure f :background *bg-col*)
    f))

(defun window (title)
  (let ((W (make-instance 'ltk:toplevel :title title)))
    (ltk:configure W :background *bg-col*)
    W))

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
                     (if (<= start (- (length lst) page-length))
                         (ltk:configure right :state :normal)
                         (ltk:configure right :state :disabled)))))
    (setf left (button 0 0 f "←" #'(lambda ()
                                     (decf start page-length)
                                     (funcall redraw))))
    (setf right (button 0 1 f "→" #'(lambda ()
                                      (incf start page-length)
                                      (funcall redraw))))
    (funcall redraw)
    acc))
