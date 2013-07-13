;;;; server.lisp
;;;; the server utilities

(in-package #:breakds.hunter-kit)

(defparameter *toot-output* nil)

(defparameter *result-limit* 20000)


(define-easy-handler (search-handler :uri "/hunterkit/search") ()
    (setf (content-type*) "application/json")
    (setf *output* (raw-post-data :force-text t))
    (let ((json-obj (jsown:parse (raw-post-data :force-text t)))
          (s (start-session)))
      ;; (acceptor-log-message *acceptor* :info "~a~%" json-obj)
      (setf (session-value 'time) (get-internal-real-time))
      (let ((cur-prelim (search-main (mapcar (lambda (x) (list (jsown:val x "id")
                                                               (jsown:val x "points")))
                                             (jsown:val json-obj "req")))))
        (if (count-or-exceed cur-prelim *result-limit*)
            (let ((ungrouped (get-armor-list cur-prelim)))
              (acceptor-log-message *acceptor* :info "count: ~a~%" (length ungrouped))
              (setf (session-value 'total-entries) (length ungrouped))
              (setf (session-value 'result s)
                    (group ungrouped (jsown:val json-obj "perPage")))
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
                                     (car (session-value 'result)))))))))


(define-easy-handler (meta-handler :uri "/hunterkit/meta") ()
  (setf (content-type*) "application/json")
  (setf *output* (raw-post-data :force-text t))
  (let ((s (start-session)))
    (jsown:to-json (list :obj
                         (list "totalPage" (length (session-value 'result)))
                         (list "page" 0)
                         (list "totalEntries" (session-value 'total-entries))
                         (list "perPage" (session-value 'per-page))
                         (list "timeConsumption" (/ (session-value 'time) 1000.0))))))


(define-easy-handler (page-handler :uri "/hunterkit/page") ()
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

  

                        
                        

