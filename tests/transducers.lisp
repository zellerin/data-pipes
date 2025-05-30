(in-package #:data-flow-tests/tests)

(deftest push-pipe/test ()
  "Simple push pipe should end up in the defined state."
  (let ((*standard-output* (make-broadcast-stream))
        (small-buffer (make-instance 'buffer :size 140))
        (sink (make-instance 'buffer :size 4096)))
    (push-pipe (list (make-instance 'repeating-source :pattern "Hello" :count 100)
                     small-buffer
                     sink))
    (is (= 25 (buffer-size small-buffer))
        (= 475 (buffer-size sink)))))
