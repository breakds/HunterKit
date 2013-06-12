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
    (let ((json-obj (jsown:parse (raw-post-data :force-text t)))
          (s (start-session)))
      ;; (acceptor-log-message *acceptor* :info "~a~%" json-obj)
      (setf (session-value 'time) (get-internal-real-time))
      (let ((ungrouped (get-armor-list 
                        (search-armor (mapcar (lambda (x) (list (jsown:val x "id")
                                                                (jsown:val x "points")))
                                              (jsown:val json-obj "req"))))))
        (setf (session-value 'total-entries) (length ungrouped))
        (setf (session-value 'result s)
              (group ungrouped (jsown:val json-obj "perPage"))))
      (setf (session-value 'time) (- (get-internal-real-time)
                                     (session-value 'time)))
      (setf (session-value 'per-page s) (jsown:val json-obj "perPage"))
      (jsown:to-json (mapcar (lambda (x) (list :obj 
                                               (list "foot" (armor-name (nth 0 x)))
                                               (list "footJewels" (get-jewels (nth 0 x)))
                                               (list "waist" (armor-name (nth 1 x)))
                                               (list "waistJewels" (get-jewels (nth 1 x)))
                                               (list "hand" (armor-name (nth 2 x)))
                                               (list "handJewels" (get-jewels (nth 2 x)))
                                               (list "chest" (armor-name (nth 3 x)))
                                               (list "chestJewels" (get-jewels (nth 3 x)))
                                               (list "head" (armor-name (nth 4 x)))
                                               (list "headJewels" (get-jewels (nth 4 x)))
                                               (list "defense" (get-defense-sum x))))
                             (car (session-value 'result))))))

(define-easy-handler (meta-handler :uri "/meta") ()
  (setf (content-type*) "application/json")
  (setf *output* (raw-post-data :force-text t))
  (let ((s (start-session)))
    (jsown:to-json (list :obj
                         (list "totalPage" (length (session-value 'result)))
                         (list "page" 0)
                         (list "totalEntries" (session-value 'total-entries))
                         (list "perPage" (session-value 'per-page))
                         (list "timeConsumption" (/ (session-value 'time) 1000.0))))))


(define-easy-handler (page-handler :uri "/page") ()
    (setf (content-type*) "application/json")
    (setf *output* (raw-post-data :force-text t))
    (let ((json-obj (jsown:parse (raw-post-data :force-text t)))
          (s (start-session)))
      ;; (acceptor-log-message *acceptor* :info "~a~%" json-obj)
      (jsown:to-json (mapcar (lambda (x) (list :obj 
                                               (list "foot" (armor-name (nth 0 x)))
                                               (list "footJewels" (get-jewels (nth 0 x)))
                                               (list "waist" (armor-name (nth 1 x)))
                                               (list "waistJewels" (get-jewels (nth 1 x)))
                                               (list "hand" (armor-name (nth 2 x)))
                                               (list "handJewels" (get-jewels (nth 2 x)))
                                               (list "chest" (armor-name (nth 3 x)))
                                               (list "chestJewels" (get-jewels (nth 3 x)))
                                               (list "head" (armor-name (nth 4 x)))
                                               (list "headJewels" (get-jewels (nth 4 x)))
                                               (list "defense" (get-defense-sum x))))
                             (nth (jsown:val json-obj "page") (session-value 'result))))))

  

                        
                        

