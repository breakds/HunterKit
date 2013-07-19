;;; search.lisp
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

(defparameter *jewels* nil)

(defparameter *helms* nil)

(defparameter *chests* nil)

(defparameter *gloves* nil)

(defparameter *belts* nil)

(defparameter *boots* nil)

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
    (setf *skills* (load-skills (merge-pathnames "skills.lisp.data"
						 data-dir)))
    (setf *jewels* (load-jewels (merge-pathnames "jewel.lisp.data"
						 data-dir)))
    (setf *helms* (load-armor-list (merge-pathnames "head.lisp.data"
						    data-dir)))
    (setf *chests* (load-armor-list (merge-pathnames "chest.lisp.data"
						     data-dir)))
    (setf *gloves* (load-armor-list (merge-pathnames "hand.lisp.data"
						     data-dir)))
    (setf *belts* (load-armor-list (merge-pathnames 
				    "../data/waist.lisp.data"
				    data-dir)))
    (setf *boots* (load-armor-list (merge-pathnames 
				    "../data/foot.lisp.data"
				    data-dir)))
    (setf *armor-set* (make-array 5 :initial-contents (list *helms*
							    *chests*
							    *gloves*
							    *belts*
							    *boots*)))
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
  (if (null set-a)
      t
      (if (>= (car set-a) (car set-b))
          (set-geq (cdr set-a) (cdr set-b))
          nil)))




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
            (eq (armor-weapon item) 'both)
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


(defun head-n (lst n)
  #f
  (labels ((head-n-iter (lst n accu)
             #f
             (if (or (= n 0) (null lst))
                 (nreverse accu)
                 (head-n-iter (rest lst)
                              (- n 1)
                              (cons (car lst) accu)))))
    (head-n-iter lst n nil)))

(defmacro cut-key (key)
  `(head-n ,key 3))




(defun sieve (skill-set lst bundles &optional (weapon 'both))
  #f
  (let ((hash (make-hash-table :test #'equal)))
    (loop for item in lst
       do (awhen (cut-key (qualify-skill-set item skill-set weapon))
            (push item (gethash it hash))
            (loop for hole from 1 to (armor-holes item)
               do (loop for bundle in (aref bundles hole)
                     do (let ((key (mapcar #'+ it
                                           (cut-key (jewel-combo-key bundle)))))
                          (push (embed item bundle)
                                (gethash key hash)))))))
    hash))


(defun merge-combo-set (combo-set-a combo-set-b)
  #f
  (let ((hash (make-hash-table :test #'equal)))
    (loop 
       for key-a being the hash-keys of combo-set-a
       for val-a being the hash-value of combo-set-a
       do (loop 
             for key-b being the hash-keys of combo-set-b
             for val-b being the hash-value of combo-set-b
             do (let ((key (mapcar #'+ key-a key-b))
                      (pair (list val-b val-a)))
                  (push pair (gethash key hash)))))
    hash))



(defun search-armor (req-set &optional (weapon 'both) (white-list nil))
  "search for the set of armors that meets the req-set"
  #f
  (multiple-value-bind (skill-set thresh-set) (unzip-pair-list req-set)
    (let ((bundles (generate-jewel-bundles skill-set))
          (armor-set (aif white-list it *armor-set*)))
      ;; search-iter returns a hash table of resulting combos
      (let ((potential (labels ((search-iter (k accu)
                                  #f
				  (let ((merged (merge-combo-set 
						 accu
						 (sieve skill-set 
							(aref armor-set k)
							bundles
							weapon))))
				    (if (= k 4)
					merged
					(search-iter (1+ k) merged)))))
			 (search-iter 1 (sieve skill-set 
					       (aref *armor-set* 0) ;; all the helms
					       bundles
					       weapon)))))
	(let ((prelim (loop 
                         for key being the hash-keys of potential
                         for value being the hash-values of potential
			 when (set-geq key thresh-set) ;; if meet the requirement
			 collect value)))
	  prelim)))))


;;;; the following functions perform on prelims/prelim-lists

(defun decombo (lst)
  #f
  (if (consp (car lst))
      (let (res)
        (loop for pair in lst
           do (let ((decomposed (decombo (cadr pair))))
                (loop for armor-item in (car pair)
                   do (loop for d in decomposed 
                         do (push (cons armor-item d) res)))))
        res)
      `(,lst)))


(defun get-armor-list (prelim)
  (mapcan (lambda (x) (decombo x)) prelim))

(defun gen-points-calculator (skill-id)
  (let ((jewel-points (make-array (length *jewels*)
                                  :adjustable nil
                                  :displaced-to nil
                                  :fill-pointer nil
                                  :initial-element 0)))
    (loop for j in *jewels*
       do (let ((slot (find skill-id 
                            (carriable-skills j)
                            :key #'car)))
            (when slot
              (setf (aref jewel-points (carriable-id j))
                    (cadr slot)))))
    (lambda (armor-item)
      (let ((slot (find skill-id 
                        (carriable-skills armor-item)
                        :key #'car)))
        (+ (or (cadr slot) 0)
           (if (is-stuffed armor-item)
               (reduce (lambda (y x)
                         (+ (aref jewel-points x) y))
                       (stuffed-armor-jewels armor-item)
                       :initial-value 0)
               0))))))





(defun prelim-filter (get-val threshold prelim)
  (labels ((classify-armors (armor-list hash)
             #f
             (loop for armor-item in armor-list
                do (push armor-item 
                         (gethash (funcall get-val armor-item) hash))))
           (merge-hashes (armor-hash sub-hash res)
             #f
             (loop 
                for key-a being the hash-keys of armor-hash
                for val-a being the hash-values of armor-hash
                do (loop 
                      for key-b being the hash-keys of sub-hash
                      for val-b being the hash-values of sub-hash
                      do (push (list val-a val-b)
                               (gethash (+ key-a key-b) res)))))
           (skill-filter-iter (lst hash)
             #f
             (if (consp (car lst))
                 (loop for pair in lst
                    do (let ((sub-hash (make-hash-table)))
                         ;; hasing sub list
                         (skill-filter-iter (cadr pair) sub-hash)
                         ;; hashing armors 
                         (let ((armor-hash (make-hash-table)))
                           (classify-armors (car pair) armor-hash)
                           ;; (format t "~a x ~a~%" 
                           ;;         (hash-table-count sub-hash)
                           ;;         (hash-table-count armor-hash))
                           (merge-hashes armor-hash sub-hash hash))))
                 (classify-armors lst hash))))
    (let ((new-prelim (make-hash-table)))
      (loop for item in prelim
         do (skill-filter-iter item new-prelim))
      (loop 
         for key being the hash-keys of new-prelim
         for val being the hash-values of new-prelim
         when (>= key threshold)
         collect val))))




(defun prelim-drop-armor (part-id spec-id prelim)
  (labels ((drop-armor-iter (lst k)
             #f
             (if (= k part-id)
                 (if (consp (car lst))
                     (remove-if #`,(null (car x1))
                                (loop for pair in lst
                                   collect (cons (remove-if #`,(= spec-id (armor-id x1) )
                                                            (car pair))
                                                 (cdr pair))))
                     (remove-if #`,(= spec-id (armor-id x1)) lst))
                 (remove-if-not #`,(cadr x1)
                                (mapcar (lambda (pair) (cons (car pair)
                                                             (drop-armor-iter (cadr pair) 
                                                                              (1- k))))
                                        lst)))))
    (remove-if #'null (mapcar (lambda (x) (drop-armor-iter x 4)) prelim))))









(defun count-or-exceed (prelim-list &optional (limit nil))
  "count the sets in a prelim list, or return nil if the count exceed
  the limit"
  #f
  (let ((accu 0))
    (block exceeded
      (let ((check (alambda (lst multiplier)
                     #f
                     (if (consp (car lst))
                         (loop for pair in lst
                            do (self (cadr pair) 
                                     (* multiplier (length (car pair)))))
                         (progn
                           (incf accu (* multiplier (length lst)))
                           (when (and limit (> accu limit))
                             (return-from exceeded nil)))))))
        (loop for lst in prelim-list
           do (funcall check lst 1)))
      accu)))               



(defun search-main (req-set &optional (def-req 0) (weapon 'both) (white-list nil))
  #f
  (let ((prelim (search-armor req-set weapon white-list)))
    (let ((prelim-prime (if (> (length req-set) 3)
                            (reduce (lambda (y x)
                                      (prelim-filter (gen-points-calculator (car x)) (cadr x) y))
                                    (nthcdr 3 req-set)
                                    :initial-value prelim)
                            prelim)))
      (if (> def-req 0)
          (prelim-filter #'armor-def-max def-req prelim-prime)
          prelim-prime))))


;;; for user interfaces

(defun get-jewels (armor-item)
  (if (is-stuffed armor-item)
      (mapcar (lambda (x)
                (carriable-name (nth x *jewels*)))
              (stuffed-armor-jewels armor-item))
      nil))

;; (defun get-jewels (armor-item)
;;   (if (is-stuffed armor-item)
;;       (format nil "~{~a ~}"
;;               (mapcar (lambda (x)
;;                         (carriable-name (nth x *jewels*)))
;;                       (stuffed-armor-jewels armor-item)))
;;       ""))


(defun get-defense-sum (armor-list)
  (reduce (lambda (y x) (+ (armor-def-max x) y))
          armor-list :initial-value 0))


(defun grow-white-list (ind-array &key (color :white))
  (labels ((grow-iter-white (ind lst accu)
             (if (or (null lst) (null ind))
                 accu
                 (if (= (armor-id (car lst)) (car ind))
                     (grow-iter-white (cdr ind) (cdr lst) 
                                      (cons (car lst) accu))
                     (grow-iter-white ind (cdr lst)
                                      accu))))
           (grow-iter-black (ind lst accu)
             (if (null lst)
                 accu
                 (if (or (null ind)
                         (not (= (armor-id (car lst))
                                 (car ind))))
                     (grow-iter-black ind (cdr lst)
                                      (cons (car lst) accu))
                     (grow-iter-black (cdr ind) (cdr lst)
                                      accu)))))
    (let ((white-list (make-array 5 :initial-element nil)))
      (loop for i below 5
         do (setf (aref white-list i)
                  (if (eq color :white)
                      (grow-iter-white (aref ind-array i)
                                       (aref *armor-set* i)
                                       nil)
                      (grow-iter-black (aref ind-array i)
                                       (aref *armor-set* i)
                                       nil))))
      white-list)))










