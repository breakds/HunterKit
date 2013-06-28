;;;; armor-list.lisp
;;;; widget: armor list 

(in-package #:breakds.hunter-kit)

;; todo color and rare

(def-model single-armor-set-model
    ((defaults (lambda ()
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
    ((tag-name "tr")
     (template (tmpl-from "single-armor-set.tmpl"))
     (initialize (lazy-init
                    ((@ ($ (@ this parent-node)) append) (@ ((@ this render)) el))
                    nil))
     (render (lambda ()
                (render-from-model)
                
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
    ((defaults (lambda ()
                  (create 
                   page 0
                   per-page 20
                   total-page 0
                   total-entries 0
                   time-consumption 0)))
     (initialize (lazy-init
                    (setf (@ this list url) (@ args url))
                    nil))
     (model single-armor-set-model)))

(def-collection-view armor-sets-table
    ((tag-name "div")
     (template (tmpl-from "armor-sets-table.tmpl"))
     (events (create 
                "click .prev-btn" "prevBtn"
                "click .next-btn" "nextBtn"))
     (sub-view single-armor-set)
     (initialize (lazy-init
                    ((@ ($ (@ this parent-node)) append) (@ ((@ this render)) el))
                    ((@ this model list each) (@ this lazy-add))
                    ((@ this listen-to)
                     (@ this model)
                     "change"
                     (@ this render))
                    nil))
     (render (lambda ()
                (render-from-model)
                (when (equal ((@ this model get) "page") 0)
                    (@. this ($ ".prev-li") (add-class "disabled")))
                this))
     (entry-point ".table")
     (prev-btn (lambda ()
                  (when (> ((@ this model get) "page") 0)
                    ((@ this model set) "page" (1- ((@ this model get) "page")))
                    (@. this model (get "vent") (trigger
                                                 "getpage"
                                                 (create sets (@ this model)))))))
     (next-btn (lambda ()
                  (when (< ((@ this model get) "page") (1- ((@ this model get) "totalPage")))
                    ((@ this model set) "page" (1+ ((@ this model get) "page")))
                    (@. this model (get "vent") (trigger
                                                 "getpage"
                                                 (create sets (@ this model)))))))))

                  
                   



                   
    
     
                   
     
    