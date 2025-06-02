(in-package #:data-flow-tests/tests)

(deftest push-pipe/test ()
  "Simple push pipe should end up in the defined state."
  (let ((*standard-output* (make-broadcast-stream)))
    (is (equalp '(0 25 475)
                (mapcar #'buffer-size
                        (push-pipe (make-pipe
                                    '((repeating-source :pattern "Hello" :count 100) ; source
                                      (buffer :size 140) ; small buffer
                                      (buffer :size 4096))))))))) ; sink

(deftest push-pipe/test-gzip ()
  "Simple push pipe should end up in the defined state."
  (let ((*standard-output* (make-broadcast-stream)))
    (mapcar #'buffer-size
            (push-flush-pipe (make-pipe
                              `((random-source :count 100 :randomness 16 :base ,(make-array 30)) ; source
                                (gzip-encoder :buffer ,(make-instance 'buffer :size 4096))
                                (buffer :size 40960)))))))
