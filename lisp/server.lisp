;;;; server.lisp
;;;; the server utilities

(in-package #:breakds.hunter-kit)

(defparameter *toot-output* nil)

(define-easy-handler (test-handler-a :uri "/testa") ()
  (setf (content-type*) "application/json")
  (setf *output* (raw-post-data :force-text t))
  (let ((json-obj (jsown:parse (raw-post-data :force-text t))))
    (let ((s (start-session)))
      (acceptor-log-message *acceptor* :info "session id: ~a~%" (session-id s))
      (jsown:to-json '(:obj ("a" 12) ("b" 25))))))
