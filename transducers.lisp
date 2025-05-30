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

(defgeneric transduced-read-chunk (transducer chunk-size)
  (:documentation "Read chunk of data from transducer if available, or return nil"))

(defgeneric transducer-flush-read (transducer)
  (:documentation "Return all the data from the transducer "))

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
   (end       :accessor get-end       :initarg :end))
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

(defun buffer-size (buffer)
  (with-slots (start end) buffer
    (- end start)))

(defmethod transducer-write (new-data (buffer buffer))
  (let ((data-size (length new-data)))
    (with-slots (start end data) buffer
      (cond
        ((>= (length data) (+ data-size end))
         (replace data new-data :start1 end)
         (incf end data-size)
         data-size)
        (t (error "Implement me: data do not fit the buffer, but maybe still could."))))))

(defmethod transducer-full-read ((buffer buffer))
  (with-slots (data start end threshold) buffer
    (when (>= (- end start) threshold)
      (let ((old-start start)
            (old-end end))
        (setf start 0 end 0)
        (values data old-start old-end)))))

(defsection @source (:title "Simple data source")
  (repeating-source class))

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

(defsection @sink (:title "Simple data sink"))

(defmethod transducer-write (new-data (sink (eql :sink)))
  (print new-data))

(defsection @pusher (:title "Pusher")
  (push-pipe function))

(defun push-pipe (pipe)
  (when (cdr pipe)
    (loop
      with data and  start and end
      do (multiple-value-setq (data start end) (transducer-full-read (car pipe)))
      while data
      do
         (format t "~&~40a --- ~4d --> ~20a ~%" (car pipe) (- end start) (second pipe))
         (transducer-write (subseq data start end) (second pipe))
         (push-pipe (cdr pipe)))))

(defmethod get-name ((transducer transducer))
  (class-name (class-of transducer)))
