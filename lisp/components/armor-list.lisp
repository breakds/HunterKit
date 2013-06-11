;;;; armor-list.lisp
;;;; widget: armor list 

(in-package #:breakds.hunter-kit)

;; todo color and rare

(def-model single-armor-set-model
    (('defaults '(lambda ()
                  (create 
                   defense 0
                   head ""
                   head-jewels ""
                   chest ""
                   chest-jewels ""
                   hand ""
                   hand-jewels ""
                   waist ""
                   waist-jewels ""
                   foot ""
                   foot-jewels "")))))

(def-view single-armor-set
    (('tag-name "tr")
     ('template `,(read-tmpl "single-armor-set.tmpl"))
     ('initialize '(lazy-init
                    ((@ ($ (@ this parent-node)) append) (@ ((@ this render)) el))
                    nil))
     ('render '(lambda ()
                (render-from-model)
                
                (trace ((@ this model get) "headJewels"))

                (if (eql ((@ this model get) "headJewels") "")
                    (@. this ($ ".head-slot") (add-class "btn-primary disabled"))
                    (progn
                      (@. this ($ ".head-slot") (popover))
                      (@. this ($ ".head-slot") (add-class "btn-success"))))

                (if (eql ((@ this model get) "chestJewels") "")
                    (@. this ($ ".chest-slot") (add-class "btn-primary disabled"))
                    (progn
                      (@. this ($ ".chest-slot") (popover))
                      (@. this ($ ".chest-slot") (add-class "btn-success"))))

                (if (eql ((@ this model get) "handJewels") "")
                    (@. this ($ ".hand-slot") (add-class "btn-primary disabled"))
                    (progn
                      (@. this ($ ".hand-slot") (popover))
                      (@. this ($ ".hand-slot") (add-class "btn-success"))))
                
                (if (eql ((@ this model get) "waistJewels") "")
                    (@. this ($ ".waist-slot") (add-class "btn-primary disabled"))
                    (progn
                      (@. this ($ ".waist-slot") (popover))
                      (@. this ($ ".waist-slot") (add-class "btn-success"))))

                (if (eql ((@ this model get) "footJewels") "")
                    (@. this ($ ".foot-slot") (add-class "btn-primary disabled"))
                    (progn
                      (@. this ($ ".foot-slot") (popover))
                      (@. this ($ ".foot-slot") (add-class "btn-success"))))
                
                this))))

(def-collection armor-sets
    (('defaults '(lambda ()
                  (create 
                   page 0
                   total 0
                   max-entries 20)))
     ('initialize '(lazy-init
                    (setf (@ this list url) (@ args url))
                    nil))
     ('model 'single-armor-set-model)))

(def-collection-view armor-sets-table
    (('tag-name "div")
     ('template `,(read-tmpl "armor-sets-table.tmpl"))
     ('sub-view 'single-armor-set)
     ('initialize '(lazy-init
                    ((@ ($ (@ this parent-node)) append) (@ ((@ this render)) el))
                    ((@ this model list each) (@ this lazy-add))
                    nil))
     ('render '(lambda ()
                (render-from-model)
                this))
     ('entry-point ".table")))



                   
    
     
                   
     
    