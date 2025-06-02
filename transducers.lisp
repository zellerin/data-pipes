(in-package #:data-flow-tests)

(defsection @overview ()
  "")

(defgeneric transducer-write (data transducer)
  (:documentation
   "Write DATA to the transducer. Return number of written octets. DATA must be
copied if needed outside of functions dynamix extent."))

(defgeneric transducer-full-read (transducer)
  (:documentation "Read DATA from the transducer. Return data read.

This does not allow specification of the limited data request, so you have the
data and should use them in full."))

(defgeneric transducer-read-chunk (transducer chunk-size)
  (:documentation "Read chunk of data from transducer if available, or return nil"))

(defgeneric transducer-flush-read (transducer)
  (:documentation "Return all the data from the transducer ")
  (:method (transducer) (transducer-full-read transducer)))

(defclass transducer ()
  ())

(defsection @buffer (:title "Buffer")
  "Buffers accept data in small chunks and report them in a bigger block."
  (buffer class)
  (buffer-size function))

(defclass buffer (transducer)
  ((data      :accessor get-data      :initarg :data)
   (threshold :accessor get-threshold :initarg :threshold)
   (start     :accessor get-start     :initarg :start)
   (end       :accessor get-end       :initarg :end)
   (flushed   :accessor get-flushed   :initarg :flushed
              :initform nil))
  (:documentation
   "A buffer of size specified at the creation time and a threshold. The data are
not released unless there is at least THRESHOLD of them.")
  (:default-initargs :start 0 :end 0))

(defmethod initialize-instance :after ((buffer buffer) &key size (threshold (round (* size 2/3))))
  (setf (slot-value buffer 'data) (make-array size)
        (slot-value buffer 'threshold) threshold))

(defmethod print-object ((buffer buffer) out)
  (print-unreadable-object (buffer out :type t)
    (with-slots (start end data threshold) buffer
        (format out "~d of ~d, threshold ~d" (- end start) (length data) threshold))))

(defgeneric buffer-size (buffer)
  (:method (buffer) 0)
  (:method ((buffer buffer))
    (with-slots (start end) buffer
      (- end start))))

(defmethod transducer-write (new-data (buffer buffer))
  (let ((data-size (length new-data)))
    (with-slots (start end data) buffer
      (cond
        ((>= (length data) (+ data-size end))
         (replace data new-data :start1 end)
         (incf end data-size)
         data-size)
        (t (error "Implement me: data do not fit the buffer, but maybe still could."))))))

(defgeneric transducer-flush (o)
  (:method (o))
  (:method ((b buffer))
    (setf (slot-value b 'flushed) t)))

(defmethod transducer-full-read ((buffer buffer))
  (with-slots (data start end threshold flushed) buffer
    (when (or (and flushed (> end start))
              (>= (- end start) threshold))
      (let ((old-start start)
            (old-end end))
        (setf start 0 end 0)
        (values data old-start old-end)))))

(defsection @source (:title "Simple data source")
  (repeating-source class)
  (random-source class))

(defclass repeating-source (transducer)
  ((pattern :accessor get-pattern :initarg :pattern)
   (count   :accessor get-count   :initarg :count)))

(defmethod transducer-full-read ((source repeating-source))
  (with-slots (pattern count) source
    (unless (minusp (decf count))
      (values pattern 0 (length pattern)))))

(defmethod print-object ((object repeating-source) out)
  (print-unreadable-object (object out :type t)
    (with-slots (pattern count) object
      (format out "~d×~a" count pattern))))

(defclass random-source ()
  ((base       :accessor get-base       :initarg :base)
   (count      :accessor get-count      :initarg :count)
   (randomness :accessor get-randomness :initarg :randomness))
  (:default-initargs :randomness 64))

(defmethod transducer-full-read ((source random-source))
  (with-slots (base count randomness) source
    (unless (minusp (decf count))
      (map-into base (lambda () (random randomness)))
      (values base 0 (length base)))))

(defsection @sink (:title "Simple data sink"))

(defmethod transducer-write (new-data (sink (eql :sink)))
  (print new-data))

(defsection @pusher (:title "Pusher")
  (push-pipe function)
  (push-flush-pipe function))

(defun push-pipe (pipe)
  (when (cdr pipe)
    (loop
      with data and  start and end
      do (multiple-value-setq (data start end) (transducer-full-read (car pipe)))
      while data
      do
         (format t "~&~40a --- ~4d --> ~20a ~%" (car pipe) (- end start) (second pipe))
         (transducer-write (subseq data start end) (second pipe))
         (push-pipe (cdr pipe)))
    pipe))

(defun push-flush-pipe (pipe)
  (when (cdr pipe)
    (transducer-flush (car pipe))
    (push-pipe pipe)
    (push-flush-pipe (cdr pipe))
    pipe))

(defsection @gzip (:title "gzip decoder and encoder")
  (gzip-encoder class))

(defclass buffer-as-output-stream (trivial-gray-streams:fundamental-output-stream)
  ((buffer :accessor get-buffer :initarg :buffer)))

(defclass gzip-encoder (buffer-as-output-stream)
  ((stream :accessor get-stream :initarg :stream)))

(defmethod transducer-full-read ((transducer buffer-as-output-stream))
  (transducer-full-read (get-buffer transducer)))

(defmethod trivial-gray-streams:stream-write-sequence ((buffer buffer-as-output-stream)
                                                       sequence start end &key &allow-other-keys)
  (transducer-write (subseq sequence start end) (get-buffer buffer)))

(defmethod initialize-instance :after ((encoder gzip-encoder) &key)
  (setf (slot-value encoder 'stream) (gzip-stream:make-gzip-output-stream encoder)))

(defmethod transducer-write (data (encoder gzip-encoder))
  (write-sequence data (get-stream encoder)))

(defmethod transducer-flush ((transducer gzip-encoder))
  ;; this should write the data to the underlying buffer
  (trivial-gray-streams:stream-finish-output (get-stream transducer))
  (transducer-flush (get-buffer transducer)))

(defsection @pipes (:title "Simplified pipe creation")
  (make-pipe function))

(defun make-pipe (description)
  "Make a pipe matching DESCRIPTION. The parameter is a list where each item is of form
CLASS-NAME or (CLASS-NAME initialization-parameter ...)"
  (loop for item in description
        unless (listp item)
          do (setf item (list item))
        collect
           (apply #'make-instance (car item) (rest item))))
