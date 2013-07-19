;;;; armor-list.lisp
;;;; widget: armor list 

(in-package #:breakds.hunter-kit)

;; todo color and rare

(def-model single-armor-set-model
    ((defaults (lambda ()
                  (create 
                   defense 0
                   head ""
                   head-jewels (make-array)
                   chest ""
                   chest-jewels (make-array)
                   hand ""
                   hand-jewels (make-array)
                   waist ""
                   waist-jewels (make-array)
                   foot ""
                   foot-jewels (make-array))))))

(def-view single-armor-set
    ((tag-name "div")
     (template (tmpl-from "armor-set-card.tmpl"))
     (initialize (lazy-init
                  (if (@ args fancy-add)
                      (@. ($ (@ this parent-node)) 
                          (append (@. this (render) el))
                          (masonry "appended" (make-array (@. this el))))
                      ((@ ($ (@ this parent-node)) append) (@ ((@ this render)) el)))
                  nil))
     (render (lambda ()
                (render-from-model)
                (@. this $el (add-class "item thumbnail"))
                (@. this $el (css "margin-bottom" 10))
                (@. this $el (css "box-shadow" "10px 5px 15px rgba(0,0,0,.5)"))
                (@. this ($ ".armor-td") (css "vertical-align" "middle"))

                
                (macrolet ((handle-part (part)
                             `(progn
                                (dolist (x (@. this model (get ,(exmac:mkstr part "Jewels"))))
                                  (@. this ($ ,(exmac:mkstr "." part "-jewels"))
                                      (append (+ "<span class=\"label label-important\">"
                                                 x
                                                 "</span><br>"))))
                                (@. this ($ ,(exmac:mkstr "." part "-slot"))
                                    (add-class "btn-info"))
                                (@. this ($ ,(exmac:mkstr "." part "-slot"))
                                    (css "box-sizing" "border-box"))
                                (@. this ($ ,(exmac:mkstr "." part "-slot"))
                                    (css "width" "100%")))))
                  (handle-part "head")
                  (handle-part "chest")
                  (handle-part "hand")
                  (handle-part "waist")
                  (handle-part "foot"))
                this))))

(def-collection armor-sets
    ((defaults (lambda ()
                  (create 
                   page 0
                   per-page 24
                   total-page 0
                   total-entries 0
                   time-consumption 0)))
     (initialize (lazy-init
                    (setf (@ this list url) (@ args url))
                    nil))
     (model single-armor-set-model)))

(def-collection-view armor-sets-table
    ((tag-name "div")
     (template (tmpl-from "armor-set-exhibition.tmpl"))
     (sub-view single-armor-set)
     (entry-point ".cascade")
     (events (create "click .continue" "continue"))
     (initialize (lazy-init
                  (setf (@ this init-push) true)
                  ((@ ($ (@ this parent-node)) append) (@ ((@ this render)) el))
                  ((@ this model list each) (@ this lazy-add))
                  (if (> (@ this model list length) 0)
                      (@. this ($ ".cascade") (masonry (create item-selector ".item"
                                                               gutter 10)))
                      (@. this ($ ".continue") (add-class "disabled")))
                  (setf (@ this init-push) false)
                  nil))
     (lazy-add (lambda (model)
                 (defvar view nil)
                 (let ((parent-node (@. this ($ ".cascade")))
                       (fancy-add (not (@ this init-push))))
                   (setf view (new ((@ this sub-view) 
                                    (create model model 
                                            fancy-add fancy-add
                                            parent-node parent-node))))
                   (setf (getprop (@ this view-list) (@ model cid)) view)
                   view)))
     (render (lambda ()
               (render-from-model)
                this))
     (continue (lambda ()
                 (if (> (@ this model list length) 0)
                     (@. this model (get "vent")
                         (trigger "getpage"
                                  (create sets (@ this model)))))
                 nil))))

                  
                   



                   
    
     
                   
     
    