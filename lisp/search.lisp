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
                                         :displaced-to nil)))))
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
      (load-list boots "foot.lisp.data")
      (setf *armor-set* (make-array 5 :initial-contents (list *helms*
                                                              *chests*
                                                              *gloves*
                                                              *belts*
                                                              *boots*))))
    (format t "[ ok ] Initialization.~%")))







;;; ------------------------------------------------------------
;;; Accessors
;;; 


(defun get-id-by-name (name lst)
  "find the entity with the given name in the lst"
  (find-if (lambda (x) (equal (entity-name x) name))
	   lst))



;;; ------------------------------------------------------------
;;; Utilities
;;; 


(defun set-geq (set-a set-b)
  "set-geq comapres two sets and returns true if set-a is greater
than or equals to set-b"
  ;; "set-a is greater than or equal to set-b" means that every
  ;; element in set-a is greater or equal than every element of set-b
  (let ((min-a (apply #'min set-a))
        (max-b (apply #'max set-b)))
    (>= min-a max-b)))



(defun unzip-pair-list (lst)
  "unizp a pair-list and return a list of heads and a list of tails"
  (labels ((unzip-iter (lst accu-head accu-tail)
             (if (null lst)
                 (values (nreverse accu-head)
                         (nreverse accu-tail))
                 (unzip-iter (rest lst)
                             (cons (caar lst) accu-head)
                             (cons (cadar lst) accu-tail)))))
    (unzip-iter lst nil nil)))




;;; ------------------------------------------------------------
;;; Search Core Code
;;; 
;;; some concepts:
;;; 1. req-set = a set of (skill-id skill-requirement) pairs
;;; 2. qualify against a req-set = the sum of skill points 
;;;    of an item is larger or equal than the specified req-set






(defun query-skill (skill-id item)
  "If the item (armor) has the specified skill, return the points;
  otherwise return 0"
  #f
  (let ((res (find-if (lambda (x) (= (car x) skill-id)) 
                      (carriable-skills item))))
    (if res
        (cadr res)
        0)))



(defun qualify-skill-set (item skill-set &optional (weapon 'both))
  "if qualify return the key, otherwise nil"
  #f
  (when (or (eq weapon 'both)
            (eq weapon (armor-weapon item)))
    (let ((key (loop for sk in skill-set
		  collect (query-skill sk item))))
      (loop for ele in key
	 when (> ele 0)
	 return key))))




;; the following 3 functions deal with jewel-combos
;;
;; 1. a jewel-combo is a (key jewel-id-list) pair, where the key is a 
;;    list of skill points generated by the jewel-id-list
;; 2. a jewel-bundle is a list of jewel-combos that occupy the same number
;;    of holes
(defun merge-jewel-combo (a b)
  "merge two jewel-combos"
  #f
  (make-jewel-combo :key (mapcar #'+ 
				 (jewel-combo-key a) 
				 (jewel-combo-key b))
		    :jewels (append (jewel-combo-jewels a)
				    (jewel-combo-jewels b))))

(defun merge-bundles (bundles j)
  "input 'bundles' if a vector of bundles, whose nth element
corresponds to the bundle of n holes. This function will update the
bundle of (1+ j) holes by merging elements from 1-hole bundle and
j-hole bundle."
  #f
  (let ((target (+ 1 j)))
    (loop for jc-i in (aref bundles 1)
       do (loop for jc-j in (aref bundles j)
	     when (or (<= (car (jewel-combo-jewels jc-i))
			  (car (jewel-combo-jewels jc-j)))
		      (and (= (length (jewel-combo-jewels jc-j)) 1)
			   (> j 1)))
	     do (push (merge-jewel-combo jc-i jc-j)
		      (aref bundles target))))))

(defun generate-jewel-bundles (skill-set)
  "return a vector of jewel-bundles, with holes of 0, 1, 2 and 3,
respectively."
  #f
  (let ((bundles (make-array 4 :initial-contents '(nil nil nil nil))))
    (loop for item in *jewels*
       for key = (qualify-skill-set item skill-set)
       when key ;; if the jewel hit at least one skill in the skill-set
       do (push (make-jewel-combo :key key
       				  :jewels (list (carriable-id item)))
       		(aref bundles (carriable-holes item))))
    ;; merge 1 + 1 holes -> 2
    (merge-bundles bundles 1)
    ;; merge 1 + 2 holes -> 3
    (merge-bundles bundles 2)
    bundles))




(defun embed (item bundle)
  "embed a jewel into an armor, the result will be a stuffed-armor"
  #f
  (make-stuffed-armor :id (armor-id item)
		      :name (armor-name item)
		      :rare (armor-rare item)
		      :weapon (armor-weapon item)
		      :def-min (armor-def-min item)
		      :def-max (armor-def-max item)
		      :gender (armor-gender item)
		      :holes (armor-holes item)
		      :skills (armor-skills item)
		      :jewels (copy-list (jewel-combo-jewels bundle))))
					 

(defun sieve (skill-set lst bundles &optional (weapon 'both))
  #f
  (let ((hash (make-hash-table :test #'equal))) 
    (loop for item in lst
       do (let ((key (qualify-skill-set item skill-set weapon)))
	    (when key ;; if the item hits at least one of the specified skills
	      (let ((obj (gethash key hash)))
                ;; if the combo exists in the hash table
                ;; let the existing combo absorb it
                ;; otherwise make it the first in the combo
		(if obj
		    (push item (combo-set obj))
		    (setf (gethash key hash) 
			  (make-combo :key key
				      :set (list item)))))
              ;; try embeding the jewels (combos)
	      (loop for hole from 1 to (armor-holes item) 
		 do (loop for bundle in (aref bundles hole)
		       do (let ((key-1 (mapcar #'+ key 
					       (jewel-combo-key bundle))))
			    (let ((obj (gethash key-1 hash)))
			      (if obj
				  (push (embed item bundle) (combo-set obj))
				  (setf (gethash key-1 hash)
					(make-combo :key key
						    :set (list 
							  (embed item
								 bundle))))))))))))
    hash))



(defun merge-combo-set (combo-set-a combo-set-b)
  #f
  (let ((hash (make-hash-table :test #'equal)))
    (loop for key-a being the hash-keys of combo-set-a
       do (loop for key-b being the hash-keys of combo-set-b
             do (let ((key (mapcar #'+ key-a key-b)))
                  (let ((obj (gethash key hash))
                        (pair (list (gethash key-a combo-set-a)
                                    (gethash key-b combo-set-b))))
                    (if obj
                        (push pair obj)
                        (setf (gethash key hash) (make-combo :key key
                                                             :set (list pair))))))))
    hash))


(defun decombo (obj)
  #f
  (loop for item in (combo-set obj)
     append (if (or (armor-p item) (stuffed-armor-p item))
                (list item)
                (loop for armor-ele in (decombo (cadr item))
                   append (loop for combo-ele in (decombo (car item))
                             collect (if (listp combo-ele)
                                         (cons armor-ele combo-ele)
                                         (list armor-ele combo-ele)))))))



(defun search-armor (req-set &optional (weapon 'both))
  "search for the set of armors that meets the req-set"
  ;; (declare (optimize (speed 3) (safety 0)))
  #f
  (multiple-value-bind (skill-set thresh-set) (unzip-pair-list req-set)
    (let ((bundles (generate-jewel-bundles skill-set)))
      ;; search-iter returns a hash table of resulting combos
      (let ((potential (labels ((search-iter (k accu)
                                  #f
				  (let ((merged (merge-combo-set 
						 accu
						 (sieve skill-set 
							(aref *armor-set* k)
							bundles
							weapon))))
				    (if (= k 4)
					merged
					(search-iter (1+ k) merged)))))
			 (search-iter 1 (sieve skill-set 
					       (aref *armor-set* 0) ;; all the helms
					       bundles
					       weapon)))))
	(let ((prelim (loop for value being the hash-values of potential
			 when (set-geq (combo-key value) thresh-set) ;; if meet the requirement
			 collect value)))
	  prelim)))))


(defun print-set (prelim)
  (let ((count 0))
    (loop for sets in prelim
       do 
	 (let ((decomboed (decombo sets)))
	   (incf count (length decomboed))
	   (loop for set in decomboed
	      do 
		(format t "==========~%") 
		(loop for item in set
		   do 
		     (format t "~a" (armor-name item))
		     (when (stuffed-armor-p item)
		       (format t "( ~{~a ~})" (mapcar (lambda (x)
							(carriable-name 
							 (nth x *jewels*)))
						      (stuffed-armor-jewels
						       item))))
		     (fresh-line)))))
    count))

(defun get-armor-list (prelim)
  (mapcan (lambda (x) (decombo x)) prelim))

(defun get-defense-sum (armor-set)
  (reduce (lambda (y x) (+ y (armor-def-max x))) armor-set
          :initial-value 0))

(defun get-jewels (armor-item)
  (if (is-stuffed armor-item)
      (format nil "~{~a ~}"
              (mapcar (lambda (x)
                        (carriable-name (nth x *jewels*)))
                      (stuffed-armor-jewels armor-item)))
      ""))
         