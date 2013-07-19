;;;; server.lisp
;;;; the server utilities

(in-package #:breakds.hunter-kit)

(defparameter *toot-output* nil)

(define-easy-handler (search-handler :uri "/hunterkit/search") ()
  (setf (content-type*) "application/json")
  (setf *output* (raw-post-data :force-text t))
  (let ((json-obj (jsown:parse (raw-post-data :force-text t)))
        (s (start-session)))
    ;; (acceptor-log-message *acceptor* :info "~a~%" json-obj)
    (setf (session-value 'time) (get-internal-real-time))
    (let ((cur-prelim (search-main (mapcar (lambda (x) (list (jsown:val x "id")
                                                             (jsown:val x "points")))
                                           (jsown:val json-obj "req"))
                                   0
                                   (if (equal (jsown:val json-obj "weapon") "saber")
                                       'saber
                                       'archer))))
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
                                                 `("footJewels" ,@(get-jewels (nth 0 x)))
                                                 (list "waist" (armor-name (nth 1 x)))
                                                 `("waistJewels" ,@(get-jewels (nth 1 x)))
                                                 (list "hand" (armor-name (nth 2 x)))
                                                 `("handJewels" ,@(get-jewels (nth 2 x)))
                                                 (list "chest" (armor-name (nth 3 x)))
                                                 `("chestJewels" ,@(get-jewels (nth 3 x)))
                                                 (list "head" (armor-name (nth 4 x)))
                                                 `("headJewels" ,@(get-jewels (nth 4 x)))
                                                 (list "defense" (get-defense-sum x))))
                               (loop 
                                  for entry = (funcall next)
                                  for count from 0
                                  until (or (not entry) (>= count per-page))
                                  collect entry)))))))


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
                                             `("footJewels" ,@(get-jewels (nth 0 x)))
                                             (list "waist" (armor-name (nth 1 x)))
                                             `("waistJewels" ,@(get-jewels (nth 1 x)))
                                             (list "hand" (armor-name (nth 2 x)))
                                             `("handJewels" ,@(get-jewels (nth 2 x)))
                                             (list "chest" (armor-name (nth 3 x)))
                                             `("chestJewels" ,@(get-jewels (nth 3 x)))
                                             (list "head" (armor-name (nth 4 x)))
                                             `("headJewels" ,@(get-jewels (nth 4 x)))
                                             (list "defense" (get-defense-sum x))))
                           (loop 
                              for entry = (funcall next)
                              for count from 0
                              until (or (not entry) (>= count per-page))
                              collect entry)))))







