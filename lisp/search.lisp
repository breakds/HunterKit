;;;; search.lisp
;;;; Armor Searcher for Monster Hunter P3


(in-package breakds.hunter-kit)


;;; data strcutures for Hunter Kit

(defstruct entity 
  "An entity is the super class of any objects in hunter-kit. Objects
  can be skills, jewels, armors or stuffed-armors"
  (id 0 :type fixnum) 
  (name "" :type string))

(defstruct (skill (:include entity)) 
  "A skill is an entity with a list of tags, where each tag is a
  triplet of (skill-points skill-name skill-description)"
  (tags nil))

(defstruct (carriable (:include entity)) 
  "A carriable is something that can be worn, including armor and
stuffed-armor"
  (holes 0 :type fixnum) (skills nil))

(defstruct (armor (:include carriable))
  (weapon 'both :type symbol) ;; value can be 'both 'saber and 'archer
  (def-min 0 :type fixnum) ;; the minimum defence value
  (def-max 0 :type fixnum) ;; the maximum defence value 
  (rare 0 :type fixnum) ;; the item's rare value
  (gender 'both :type symbol)) ;; value can be 'both 'male and 'female

(defstruct (stuffed-armor (:include armor))
  "a stuffed armor is an armor with stuffed jewels"
  (jewels nil))


(defstruct jewel-combo
  "A combination of jewels"
  (key nil)
  (jewels nil))


(defstruct combo
  "A combination of (stuffed) armors"
  (key nil)
  (set nil))


(defmethod is-stuffed ((a armor)) nil)

(defmethod is-stuffed ((a stuffed-armor)) t)





;;; global variables 

(defparameter *skills* nil)
(defparameter *skills-array* nil)

(defparameter *jewels* nil)
(defparameter *jewels-array* nil)

(defparameter *helms* nil)
(defparameter *helms-array* nil)

(defparameter *chests* nil)
(defparameter *chests-array* nil)

(defparameter *gloves* nil)
(defparameter *gloves-array* nil)

(defparameter *belts* nil)
(defparameter *belts-array* nil)

(defparameter *boots* nil)
(defparameter *boots-array* nil)

(defparameter *armor-set* nil)



;;; ------------------------------------------------------------
;;; initialization of the searcher
;;;

(defun load-skills (file-name)
  "load skill data from external file, creating a list of skill"
  (with-open-file (in file-name
                      :direction :input)
    (loop for item = (read in nil nil) while item
       collect (make-skill :id (car item)
                           :name (cadr item)
                           :tags (caddr item)))))


(defun load-jewels (file-name)
  "Load jewel data from external file, creating a list of carriable"
  (with-open-file (in file-name
                      :direction :input)
    (loop for item = (read in nil nil) while item
       collect (make-carriable :id (getf item :id)
                               :name (getf item :name)
                               :holes (getf item :holes)
                               :skills (getf item :skills)))))

(defun load-armor-list (file-name)
  "Load armor data from external file, creating a list of armor"
  (with-open-file (in file-name
                      :direction :input)
    (loop for item = (read in nil nil) while item
       collect (make-armor :id (getf item :id)
                           :name (getf item :name)
                           :rare (getf item :rare)
                           :weapon (getf item :weapon)
                           :def-min (getf item :def-min)
                           :def-max (getf item :def-max)
                           :gender (getf item :gender)
                           :holes (getf item :holes)
                           :skills (getf item :skills)))))



(defun init ()
  "load data from external files."
  (let ((data-dir (asdf:system-relative-pathname 'hunter-kit
						 "data/")))
    (macrolet ((load-list (name path)
                 (let ((lst-name (exmac:symb '* name '*)))
                   `(tagbody
                       (setf ,lst-name
                             (,(exmac:symb 'load- name) 
                               (merge-pathnames ,path data-dir)))
                       (setf ,(exmac:symb '* name '-array*)
                             (make-array (length ,lst-name)
                                         :adjustable nil
                                         :fill-pointer nil
                                         :displaced-to nil
                                         :initial-contents ,lst-name)))))
               (load-helms (&rest args)
                 `(load-armor-list ,@args))
               (load-chests (&rest args)
                 `(load-armor-list ,@args))
               (load-gloves (&rest args)
                 `(load-armor-list ,@args))
               (load-belts (&rest args)
                 `(load-armor-list ,@args))
               (load-boots (&rest args)
                 `(load-armor-list ,@args)))
      (load-list skills "skills.lisp.data")
      (load-list jewels "jewel.lisp.data")
      (load-list helms "head.lisp.data")
      (load-list chests "chest.lisp.data")
      (load-list gloves "hand.lisp.data")
      (load-list belts "waist.lisp.data")
      (load-list boots "foot.lisp.data"))
    (format t "[ ok ] Initialization.~%")))







;;; Search Routines and Helpers 

;; data structures
;; 1. search result are represent as combos
;; 2. a combo is defined as either one of the following forms
;;    - (:armor armor-id &rest jewel-ids)
;;    - (:combo &rest combos)

(defun gen-skill-filter (item-list)
  (let ((inverse-map (make-array (length *skills*)
                                 :adjustable nil
                                 :fill-pointer nil
                                 :displaced-to nil
                                 :initial-element nil)))
    (loop for item in item-list
       do (loop for skill-descr in (carriable-skills item)
             do (when (> (cadr skill-descr) 0)
                  (push (carriable-id item) 
                        (aref inverse-map (car skill-descr))))))
    (lambda (skill-id-list)
      (remove-duplicates (loop for i in skill-id-list
                            append (aref inverse-map i))))))

(macrolet ((create-skill-filters (&rest names)
             `(progn
                ,@(mapcar #`(setf (symbol-function ',(exmac:symb 'filter- x1))
                                  (gen-skill-filter ,(exmac:symb '* x1 '*)))
                          names))))
  (create-skill-filters jewels helms chests gloves belts boots))






       
  


;; (defun search-armor (req-set &key (weapon 'both) (mask nil))
;;   "main search function"
  
  
         