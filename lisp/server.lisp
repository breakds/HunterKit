;;;; server.lisp
;;;; the server utilities

(in-package #:breakds.hunter-kit)

(defparameter *toot-output* nil)



(defun make-black-list (lst)
  (let ((res (make-array 5 :initial-element nil)))
    (dolist (x lst)
      (let* ((i (position #\, x))
             (part-id (parse-integer x :start 0 :end i))
             (id (parse-integer x :start (1+ i))))
        (push id (aref res part-id))))
    res))

(defun pull-n-item (next n)
  "collect the result of calling next n times, before next returns nil"
  (loop 
     for i below n
     for entry = (funcall next)
     while entry
     collect entry))


      
                                   
                                   
                                   

(define-easy-handler (search-handler :uri "/hunterkit/search") ()
  (setf (content-type*) "application/json")
  (setf *output* (raw-post-data :force-text t))
  (let ((json-obj (jsown:parse (raw-post-data :force-text t)))
        (s (start-session)))
    (acceptor-log-message *acceptor* :info "~a~%" json-obj)
    (setf (session-value 'time) (get-internal-real-time))
    (let ((cur-prelim (search-main (mapcar (lambda (x) (list (jsown:val x "id")
                                                             (jsown:val x "points")))
                                           (jsown:val json-obj "req"))
                                   0
                                   (if (equal (jsown:val json-obj "weapon") "saber")
                                       'saber
                                       'archer)
                                   (grow-white-list (make-black-list 
                                                     (jsown:val json-obj "blackList"))
                                                    :color :black))))
      (setf (session-value 'prelim) cur-prelim)
      (setf (session-value 'time) (- (get-internal-real-time)
                                     (session-value 'time)))
      (setf (session-value 'total-entries) (count-or-exceed cur-prelim))
      (acceptor-log-message *acceptor* :info "count: ~a~%" (session-value 'total-entries))
      (setf (session-value 'next) (armor-set-generator cur-prelim))
      (setf (session-value 'per-page s) (jsown:val json-obj "perPage"))
      (let ((next (session-value 'next))
            (per-page (session-value 'per-page)))
        (jsown:to-json (mapcar (lambda (x) (list :obj 
                                                 (list "foot" (armor-name (nth 0 x)))
                                                 (list "footId" (armor-id (nth 0 x)))
                                                 `("footJewels" ,@(get-jewels (nth 0 x)))
                                                 (list "waist" (armor-name (nth 1 x)))
                                                 (list "waistId" (armor-id (nth 1 x)))
                                                 `("waistJewels" ,@(get-jewels (nth 1 x)))
                                                 (list "hand" (armor-name (nth 2 x)))
                                                 (list "handId" (armor-id (nth 2 x)))
                                                 `("handJewels" ,@(get-jewels (nth 2 x)))
                                                 (list "chest" (armor-name (nth 3 x)))
                                                 (list "chestId" (armor-id (nth 3 x)))
                                                 `("chestJewels" ,@(get-jewels (nth 3 x)))
                                                 (list "head" (armor-name (nth 4 x)))
                                                 (list "headId" (armor-id (nth 4 x)))
                                                 `("headJewels" ,@(get-jewels (nth 4 x)))
                                                 (list "defense" (get-defense-sum x))))
                               (pull-n-item next per-page)))))))


(define-easy-handler (refine-handler :uri "/hunterkit/refine") ()
  (setf (content-type*) "application/json")
  (setf *output* (raw-post-data :force-text t))
  (let ((json-obj (jsown:parse (raw-post-data :force-text t)))
        (s (start-session)))
    (acceptor-log-message *acceptor* :info "~a~%" json-obj)
    (setf (session-value 'time) (get-internal-real-time))
    (let ((cur-prelim (if (equal (jsown:val json-obj "operation") "drop")
                          (prelim-drop-armor (jsown:val json-obj "partId")
                                             (jsown:val json-obj "id")
                                             (session-value 'prelim))
                          (prelim-pin-armor (jsown:val json-obj "partId")
                                            (jsown:val json-obj "id")
                                            (session-value 'prelim)))))
      (setf (session-value 'prelim) cur-prelim)
      (setf (session-value 'time) (- (get-internal-real-time)
                                     (session-value 'time)))
      (setf (session-value 'total-entries) (count-or-exceed cur-prelim))
      (acceptor-log-message *acceptor* :info "count: ~a~%" (session-value 'total-entries))
      (setf (session-value 'next) (armor-set-generator cur-prelim))
      (setf (session-value 'per-page s) (jsown:val json-obj "perPage"))
      (let ((next (session-value 'next))
            (per-page (session-value 'per-page)))
        (jsown:to-json (mapcar (lambda (x) (list :obj 
                                                 (list "foot" (armor-name (nth 0 x)))
                                                 (list "footId" (armor-id (nth 0 x)))
                                                 `("footJewels" ,@(get-jewels (nth 0 x)))
                                                 (list "waist" (armor-name (nth 1 x)))
                                                 (list "waistId" (armor-id (nth 1 x)))
                                                 `("waistJewels" ,@(get-jewels (nth 1 x)))
                                                 (list "hand" (armor-name (nth 2 x)))
                                                 (list "handId" (armor-id (nth 2 x)))
                                                 `("handJewels" ,@(get-jewels (nth 2 x)))
                                                 (list "chest" (armor-name (nth 3 x)))
                                                 (list "chestId" (armor-id (nth 3 x)))
                                                 `("chestJewels" ,@(get-jewels (nth 3 x)))
                                                 (list "head" (armor-name (nth 4 x)))
                                                 (list "headId" (armor-id (nth 4 x)))
                                                 `("headJewels" ,@(get-jewels (nth 4 x)))
                                                 (list "defense" (get-defense-sum x))))
                               (pull-n-item next per-page)))))))


(define-easy-handler (meta-handler :uri "/hunterkit/meta") ()
  (setf (content-type*) "application/json")
  (setf *output* (raw-post-data :force-text t))
  (let ((s (start-session)))
    (jsown:to-json (list :obj
                         (list "totalEntries" (session-value 'total-entries))
                         (list "perPage" (session-value 'per-page))
                         (list "timeConsumption" (/ (session-value 'time) 1000.0))))))


(define-easy-handler (page-handler :uri "/hunterkit/page") ()
  (setf (content-type*) "application/json")
  (setf *output* (raw-post-data :force-text t))
  (let ((next (session-value 'next))
        (per-page (session-value 'per-page)))
    (jsown:to-json (mapcar (lambda (x) (list :obj 
                                             (list "foot" (armor-name (nth 0 x)))
                                             (list "footId" (armor-id (nth 0 x)))
                                             `("footJewels" ,@(get-jewels (nth 0 x)))
                                             (list "waist" (armor-name (nth 1 x)))
                                             (list "waistId" (armor-id (nth 1 x)))
                                             `("waistJewels" ,@(get-jewels (nth 1 x)))
                                             (list "hand" (armor-name (nth 2 x)))
                                             (list "handId" (armor-id (nth 2 x)))
                                             `("handJewels" ,@(get-jewels (nth 2 x)))
                                             (list "chest" (armor-name (nth 3 x)))
                                             (list "chestId" (armor-id (nth 3 x)))
                                             `("chestJewels" ,@(get-jewels (nth 3 x)))
                                             (list "head" (armor-name (nth 4 x)))
                                             (list "headId" (armor-id (nth 4 x)))
                                             `("headJewels" ,@(get-jewels (nth 4 x)))
                                             (list "defense" (get-defense-sum x))))
                           (pull-n-item next per-page)))))







