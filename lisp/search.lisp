;;;; search.lisp
;;;; Armor Searcher for Monster Hunter P3


(in-package breakds.hunter-kit)

;;; Structure for an mhp3 entity
(defstruct entity (id 0 :type fixnum) (name "" :type string))

;;; Structure for a skill
(defstruct (skill (:include entity)) (tags nil))

;;; Structure for decoration/armor which are carrriable
;;; This is also the structure for jewels
(defstruct (carriable (:include entity)) 
  (holes 0 :type fixnum) (skills nil))

;;; Structure for armors
(defstruct (armor (:include carriable))
  (weapon 'both :type symbol)
  (def-min 0 :type fixnum)
  (def-max 0 :type fixnum)
  (rare 0 :type fixnum)
  (gender 'both :type symbol))

;;; Structure for amrors with jewels on it
(defstruct (stuffed-armor (:include armor))
  (jewels nil))

;;; Structure for combinations of jewels
(defstruct jewel-combo
  (key nil)
  (jewels nil))



;;; Structure for combinations of armors
(defstruct combo
  (key nil)
  (set nil))




(defparameter *skills* nil)

(defparameter *jewels* nil)

(defparameter *helms* nil)

(defparameter *chests* nil)

(defparameter *gloves* nil)

(defparameter *belts* nil)

(defparameter *boots* nil)

(defparameter *armor-set* nil)

(defun load-skills (file-name)
  "Load skill data from external file, creating a list of skill"
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
;;; Aux

(defun list-geq (lst-a lst-b)
  (not (loop for a in lst-a 
          for b in lst-b
          when (< a b)
          return t)))


;;; ---------- Accessors ----------


(defun get-id-by-name (name lst)
  "find the entity with the given name in the lst"
  (find-if (lambda (x) (equal (entity-name x) name))
	   lst))


;;; ---------- Search ----------

(defun query-skill (skill-id item)
  "Get the skill points for a skill specified with skill-id from an
  armor (item)."
  (let ((res (find-if (lambda (x) (= (car x) skill-id)) 
                      (carriable-skills item))))
    (if res
        (cadr res)
        0)))



(defun qualify (item skill-set &optional (weapon nil))
  "if qualify return the key, otherwise nil"
  (when (or (null weapon)
	  (eq weapon 'both)
	  (eq weapon (armor-weapon item)))
    (let ((key (loop for sk in skill-set
		  collect (query-skill sk item))))
      (loop for ele in key
	 when (> ele 0)
	 return key))))


(defun merge-jewel-combo (a b)
  (make-jewel-combo :key (mapcar #'+ 
				 (jewel-combo-key a) 
				 (jewel-combo-key b))
		    :jewels (append (jewel-combo-jewels a)
				    (jewel-combo-jewels b))))
				 


;; side-effect
(defun merge-bundles (bundles j)
  "merge the list cosumes 1 holes and j holes"
  (let ((target (+ 1 j)))
    (loop for jc-i in (aref bundles 1)
       do (loop for jc-j in (aref bundles j)
	     when (or (<= (car (jewel-combo-jewels jc-i))
			  (car (jewel-combo-jewels jc-j)))
		      (and (= (length (jewel-combo-jewels jc-j)) 1)
			   (> j 1)))
	     do (push (merge-jewel-combo jc-i jc-j)
		      (aref bundles target))))))

(defun jewel-bundle (skill-set)
  "return a vector of lists, where n-th lists represent all the
  jewel combination using n holes"
  (let ((bundles (make-array 4 :initial-contents '(nil nil nil nil))))
    ;; This loop will 
    ;; 1) filter: keep all the jewels that have a non-empty
    ;; intersection with skill-set
    ;; 2) push them to the corresponding list in bundles according to
    ;; holes
    (loop for item in *jewels*
       for key = (qualify item skill-set)
       when key
       do (push (make-jewel-combo :key key
       				  :jewels (list (carriable-id item)))
       		(aref bundles (carriable-holes item))))
    ;; merge 1 + 1 holes -> 2
    (merge-bundles bundles 1)
    ;; merge 1 + 2 holes -> 3
    (merge-bundles bundles 2)
    bundles))


(defun embed (item bundle)
  "embed a jewel into an armor"
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
					 

(defun sieve (skill-set lst bundles &optional (weapon nil))
  (let ((hash (make-hash-table :test #'equal)))
    (loop for item in lst
       do (let ((key (qualify item skill-set weapon)))
	    (when key
	      (let ((obj (gethash key hash)))
		(if obj
		    (push item (combo-set obj))
		    (setf (gethash key hash) 
			  (make-combo :key key
				      :set (list item)))))
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
  (loop for item in (combo-set obj)
     append (if (or (armor-p item) (stuffed-armor-p item))
                (list item)
                (loop for armor-ele in (decombo (cadr item))
                   append (loop for combo-ele in (decombo (car item))
                             collect (if (listp combo-ele)
                                         (cons armor-ele combo-ele)
                                         (list armor-ele combo-ele)))))))
                
       



;;; requirement-set = ((skill-id requirement)*)
(defun search-armor (req-set &optional (weapon nil))
  "search for the set of armors that meet the requirement."
  (let ((skill-set (mapcar (lambda (x) (car x)) req-set))
        (thresh-set (mapcar (lambda (x) (cadr x)) req-set)))
    (let ((bundles (jewel-bundle skill-set)))
      (let ((potential (labels ((search-iter (k accu)
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
					       (aref *armor-set* 0)
					       bundles
					       weapon)))))
	(let ((prelim (loop for value being the hash-values of potential
			 when (list-geq (combo-key value) thresh-set)
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