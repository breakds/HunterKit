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


(defparameter *skills* nil)

(defun load-skills (file-name)
  "Load skill data from external file, creating a list of skill"
  (with-open-file (in file-name
                      :direction :input)
    (loop for item = (read in nil nil) while item
       collect (make-skill :id (car item)
                           :name (cadr item)
                           :tags (caddr item)))))


(defun load-jewel (file-name)
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




           
