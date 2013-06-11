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
      (jsown:to-json '((:obj ("a" 1) ("b" 12)) (:obj ("a" 1234) ("b" 12345)))))))



(define-easy-handler (test-handler-b :uri "/testb") ()
  (setf (content-type*) "application/json")
  (setf *output* (raw-post-data :force-text t))
  (let ((json-obj (jsown:parse (raw-post-data :force-text t))))
    (let ((s (start-session)))
      (acceptor-log-message *acceptor* :info "session id: ~a~%" (session-id s))
      (jsown:to-json '((:obj 
                        ("head" "萌猫儿头盔") 
                        ("hand" "萌猫儿手套")
                        ("chest" "萌猫儿胸甲")
                        ("waist" "萌猫儿超短裙")
                        ("foot" "萌猫儿长靴"))
                       (:obj 
                        ("head" "萌猫儿头盔") 
                        ("hand" "萌猫儿手套")
                        ("chest" "萌猫儿胸甲")
                        ("waist" "萌猫儿超短裙")
                        ("foot" "萌猫儿长靴"))
                       (:obj 
                        ("head" "萌猫儿头盔") 
                        ("hand" "萌猫儿手套")
                        ("chest" "萌猫儿胸甲")
                        ("waist" "萌猫儿超短裙")
                        ("foot" "萌猫儿长靴"))
                       (:obj 
                        ("head" "萌猫儿头盔") 
                        ("hand" "萌猫儿手套")
                        ("chest" "萌猫儿胸甲")
                        ("waist" "萌猫儿超短裙")
                        ("foot" "萌猫儿长靴"))
                       (:obj 
                        ("head" "萌猫儿头盔") 
                        ("hand" "萌猫儿手套")
                        ("chest" "萌猫儿胸甲")
                        ("waist" "萌猫儿超短裙")
                        ("foot" "萌猫儿长靴")))))))

(define-easy-handler (search-handler :uri "/search") ()
    (setf (content-type*) "application/json")
    (setf *output* (raw-post-data :force-text t))
    (let ((json-obj (jsown:parse (raw-post-data :force-text t))))
      (acceptor-log-message *acceptor* :info "~a~%" json-obj)
      (jsown:to-json '((:obj 
                        ("head" "萌猫儿头盔") 
                        ("hand" "萌猫儿手套")
                        ("chest" "萌猫儿胸甲")
                        ("waist" "萌猫儿超短裙")
                        ("foot" "萌猫儿长靴"))
                       (:obj 
                        ("head" "萌猫儿头盔") 
                        ("hand" "萌猫儿手套")
                        ("chest" "萌猫儿胸甲")
                        ("waist" "萌猫儿超短裙")
                        ("foot" "萌猫儿长靴"))
                       (:obj 
                        ("head" "萌猫儿头盔") 
                        ("hand" "萌猫儿手套")
                        ("chest" "萌猫儿胸甲")
                        ("waist" "萌猫儿超短裙")
                        ("foot" "萌猫儿长靴"))
                       (:obj 
                        ("head" "萌猫儿头盔") 
                        ("hand" "萌猫儿手套")
                        ("chest" "萌猫儿胸甲")
                        ("waist" "萌猫儿超短裙")
                        ("foot" "萌猫儿长靴"))
                       (:obj 
                        ("head" "萌猫儿头盔") 
                        ("hand" "萌猫儿手套")
                        ("chest" "萌猫儿胸甲")
                        ("waist" "萌猫儿超短裙")
                        ("foot" "萌猫儿长靴"))))))

  

                        
                        

