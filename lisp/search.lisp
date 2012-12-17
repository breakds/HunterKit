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
  (sets nil))




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





           
