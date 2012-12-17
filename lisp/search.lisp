;;;; search.lisp
;;;; Armor Searcher for Monster Hunter P3



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
  (gender 0 :type symbol))

;;; Structure for combination
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
  (setf *skills* (load-skills "../data/skills.lisp.data"))
  (setf *jewels* (load-jewels "../data/jewel.lisp.data"))
  (setf *helms* (load-armor-list "../data/head.lisp.data"))
  (setf *chests* (load-armor-list "../data/chest.lisp.data"))
  (setf *gloves* (load-armor-list "../data/hand.lisp.data"))
  (setf *belts* (load-armor-list "../data/waist.lisp.data"))
  (setf *boots* (load-armor-list "../data/foot.lisp.data"))
  (setf *armor-set* (make-array 5 :initial-contents (list *helms*
							  *chests*
							  *gloves*
							  *belts*
							  *boots*)))
  nil)
  

;;; ---------- Accessors ----------


  

(defun get-id-by-name (name lst)
  "find the entity with the given name in the lst"
  (find-if (lambda (x) (equal (entity-name x) name))
	   lst))


;;; ---------- Search ----------

(defun query-skill (skill-id item)
  (let ((res (find-if (lambda (x) (= (car x) skill-id)) 
                      (armor-skills item))))
    (if res
        (cadr res)
        0)))

(defun qualify (item skill-set)
  "if qualify return the key, otherwise nil"
  (let ((key (loop for sk in skill-set
                collect (query-skill sk item))))
    (loop for ele in key
       when (> ele 0)
       return key)))
  
  


(defun sieve (skill-set lst)
  (let ((hash (make-hash-table :test #'equal)))
    (loop for item in lst
       do (let ((key (qualify item skill-set)))
            (when key
              (let ((obj (gethash key hash)))
                (if obj
                    (push item (combo-set obj))
                    (setf (gethash key hash) (make-combo :key key
                                                         :set (list item))))))))
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


(defun list-geq (lst-a lst-b)
  (not (loop for a in lst-a 
          for b in lst-b
          when (< a b)
          return t)))


(defun decombo (obj)
  (loop for item in (combo-set obj)
     append (if (armor-p item)
                (list item)
                (loop for armor-ele in (decombo (cadr item))
                   append (loop for combo-ele in (decombo (car item))
                             collect (if (listp combo-ele)
                                         (cons armor-ele combo-ele)
                                         (list armor-ele combo-ele)))))))
                
       



;;; requirement-set = ((skill-id requirement)*)
(defun search-armor (req-set)
  "search for the set of armors that meet the requirement."
  (let ((skill-set (mapcar (lambda (x) (car x)) req-set))
        (thresh-set (mapcar (lambda (x) (cadr x)) req-set)))
    (let ((potential (labels ((search-iter (k accu)
                                (let ((merged (merge-combo-set 
                                               accu
                                               (sieve skill-set (aref *armor-set* k)))))
                                  (if (= k 4)
                                      merged
                                      (search-iter (1+ k) merged)))))
                       (search-iter 1 (sieve skill-set (aref *armor-set* 0))))))
      (let ((prelim (loop for value being the hash-values of potential
                       when (list-geq (combo-key value) thresh-set)
                       collect value)))
        prelim))))

    
  
  
  




           
