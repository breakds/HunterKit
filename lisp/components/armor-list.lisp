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

                (if (equal ((@ this model get) "headJewels") "")
                    (@. this ($ ".head-slot") (add-class "btn-primary disabled"))
                    (progn
                      (@. this ($ ".head-slot") (popover))
                      (@. this ($ ".head-slot") (add-class "btn-success"))))

                (if (equal ((@ this model get) "chestJewels") "")
                    (@. this ($ ".chest-slot") (add-class "btn-primary disabled"))
                    (progn
                      (@. this ($ ".chest-slot") (popover))
                      (@. this ($ ".chest-slot") (add-class "btn-success"))))

                (if (equal ((@ this model get) "handJewels") "")
                    (@. this ($ ".hand-slot") (add-class "btn-primary disabled"))
                    (progn
                      (@. this ($ ".hand-slot") (popover))
                      (@. this ($ ".hand-slot") (add-class "btn-success"))))
                
                (if (equal ((@ this model get) "waistJewels") "")
                    (@. this ($ ".waist-slot") (add-class "btn-primary disabled"))
                    (progn
                      (@. this ($ ".waist-slot") (popover))
                      (@. this ($ ".waist-slot") (add-class "btn-success"))))

                (if (equal ((@ this model get) "footJewels") "")
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
                   time-consumption 0
                   max-entries 20)))
     ('initialize '(lazy-init
                    (setf (@ this list url) (@ args url))
                    nil))
     ('model 'single-armor-set-model)))

(def-collection-view armor-sets-table
    (('tag-name "div")
     ('template `,(read-tmpl "armor-sets-table.tmpl"))
     ('events '(create 
                "click .prev-btn" "prevBtn"
                "click .next-btn" "nextBtn"))
     ('sub-view 'single-armor-set)
     ('initialize '(lazy-init
                    ((@ ($ (@ this parent-node)) append) (@ ((@ this render)) el))
                    ((@ this model list each) (@ this lazy-add))
                    ((@ this listen-to)
                     (@ this model)
                     "change"
                     (@ this render))
                    nil))
     ('render '(lambda ()
                (render-from-model)
                this))
     ('entry-point ".table")
     ('prev-btn '(lambda ()
                  ((@ this model set) "page" (1- ((@ this model get) "page")))
                  (@. this model (get "vent") (trigger
                                               "getpage"
                                               (create sets (@ this model))))))
     ('next-btn '(lambda ()
                  ((@ this model set) "page" (1+ ((@ this model get) "page")))
                  (@. this model (get "vent") (trigger
                                               "getpage"
                                               (create sets (@ this model))))))))

                  
                   



                   
    
     
                   
     
    