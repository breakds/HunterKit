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
;;    - (:combo armor list)
;;    - (&rest combos)



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
    (lambda (skill-list)
      (remove-duplicates (loop for i in skill-list
                            append (aref inverse-map i))))))

(macrolet ((create-skill-filters (&rest names)
             `(progn
                ,@(mapcar #`(setf (symbol-function ',(exmac:symb 'filter- x1))
                                  (gen-skill-filter ,(exmac:symb '* x1 '*)))
                          names))))
  (create-skill-filters jewels helms chests gloves belts boots))




(defun gen-encoder (skill-list)
  #f
  (let* ((n (length skill-list))
         (mask 0))
    (declare (type fixnum n))
    (declare (type fixnum mask))
    (loop for i below n
       do (setf mask (the fixnum 
                       (+ (ash mask 8) 32))))
    (lambda (item)
      #f
      (let ((code 0))
        (declare (type fixnum code))
        (loop for skill-id in skill-list
           do (setf code (the fixnum 
                           (+ (ash code 8) 
                              (+ 32 (or (cadr (find skill-id (carriable-skills item)
                                                   :key #'car))
                                        0))))))
        (the fixnum code)))))






(defun gen-code-combiner (skill-list)
  #f
  (let ((n (length skill-list)))
    (declare (type fixnum n))
    (let ((mask 0))
      (declare (type fixnum mask))
      (loop for i below n 
         do (setf mask (the fixnum (+ (ash mask 8) 32))))
      (lambda (x y)
        (declare (optimize (speed 3) (safety 0)))
        (declare (type fixnum x))
        (declare (type fixnum y))
        (declare (type fixnum mask))
        (the fixnum (- (+ x y) mask))))))






           
(defun init-jewel-set (combine jewel-cand)
  #f
  (let ((jewel-set (make-array 4 
                               :initial-element nil
                               :adjustable nil
                               :fill-pointer nil
                               :displaced-to nil)))
    (macrolet ((pure-n-hole (n)
                 `(remove-if-not (lambda (x)
                                   (= ,n (carriable-holes (aref *jewels-array* (cadr x)))))
                                 jewel-cand)))
      ;; 1 hole
      (setf (aref jewel-set 1) (pure-n-hole 1))
      ;; 2 holes and 3 holes
      (macrolet ((cartesian (a b)
                   (exmac:with-gensyms (i j)
                     `(loop for ,i in ,a
                         append (loop for ,j in ,b
                                   collect (cons (funcall combine 
                                                          (car ,i) 
                                                          (car ,j))
                                                 (append (cdr ,i)
                                                         (cdr ,j))))))))
        ;; 2 holes
        (setf (aref jewel-set 2) 
              (append (aref jewel-set 1)
                      (pure-n-hole 2)
                      (delete-if (lambda (x) (> (cadr x) (caddr x)))
                                 (cartesian (aref jewel-set 1)
                                            (aref jewel-set 1)))))
        ;; 3 holes
        (setf (aref jewel-set 3)
              (append (aref jewel-set 1)
                      (aref jewel-set 2)
                      (pure-n-hole 3)
                      (cartesian (aref jewel-set 1)
                                 (aref jewel-set 2))))
        jewel-set))))
                      
                      
          
                  
  

                    
    
  
  


(defun search-armor (req-set)
  "main search function"
  (let* ((skill-list (mapcar #`,(car x1) req-set))
         (encode (gen-encoder skill-list))
         (combine (gen-code-combiner skill-list))
         (jewel-cand (mapcar #`(,(funcall encode (aref *jewels-array* x1))
                                 ,x1)
                             (filter-jewels skill-list)))
         (jewel-set (init-jewel-set combine jewel-cand)))
    (macrolet ((prepare-armor-list (part)
                 (exmac:with-gensyms (item-id jewel-list item item-code)
                   `(loop for ,item-id in (,(exmac:symb 'filter- part) skill-list)
                       append (let* ((,item (aref ,(exmac:symb '* part '-array*) ,item-id))
                                     (,item-code (funcall encode ,item)))
                                (cons `(,,item-code ,,item-id)
                                      (loop for ,jewel-list in 
                                           (aref jewel-set (carriable-holes ,item))
                                         collect (cons (funcall combine 
                                                                ,item-code
                                                                (car ,jewel-list))
                                                       (cadr ,jewel-list))))))))
               (cascade (part from)
                 (exmac:with-gensyms (every res candidates)
                   `(let ((,res (make-hash-table :test #'eq))
                          (,candidates (prepare-armor-list ,part)))
                      (maphash (lambda (code lst)
                                 (loop for ,every in ,candidates
                                    do (let ((new-code (funcall combine 
                                                                code 
                                                                (car ,every))))
                                         (push (list :combo
                                                     (cdr ,every)
                                                     lst)
                                               (gethash new-code ,res)))))
                               ,from)
                      ,res))))
      (cascade helms 
               (cascade chests
                        (cascade gloves
                                 (cascade belts
                                          (let ((res (make-hash-table :test #'eq)))
                                            (loop for every in (prepare-armor-list boots)
                                               do (push (cdr every)
                                                        (gethash (car every) res)))
                                            res))))))))
                                             
                                         
                   

      
    
                 
                                      

                                      
                                      
  
  
  
         