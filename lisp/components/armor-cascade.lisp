;;;; armor-cascade.lisp
;;;; widget: armor list 

(in-package #:breakds.hunter-kit)

;; todo color and rare

(def-model single-armor-set-model
    ((defaults (lambda ()
                 (create 
                  defense 0
                  head ""
                  head-jewels (make-array)
                  head-id 0
                  chest ""
                  chest-jewels (make-array)
                  chest-id 0
                  hand ""
                  hand-jewels (make-array)
                  hand-id 0
                  waist ""
                  waist-jewels (make-array)
                  waist-id 0
                  foot ""
                  foot-jewels (make-array)
                  foot-id 0)))))

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
     (events (create "click .head-slot" "refineHead"
                     "click .chest-slot" "refineChest"
                     "click .hand-slot" "refineHand"
                     "click .waist-slot" "refineWaist"
                     "click .foot-slot" "refineFoot"))
     (refine-head (lambda ()
                    (@. this model collection 
                        parent-model (get "vent")
                        (trigger "refine" (create title (@. this model (get "head"))
                                                  part-id 0
                                                  id (@. this model (get "headId")))))
                    nil))
     (refine-chest (lambda ()
                     (@. this model collection 
                         parent-model (get "vent")
                         (trigger "refine" (create title (@. this model (get "chest"))
                                                   part-id 1
                                                   id (@. this model (get "chestId")))))
                     nil))
     (refine-hand (lambda ()
                    (@. this model collection 
                        parent-model (get "vent")
                        (trigger "refine" (create title (@. this model (get "hand"))
                                                  part-id 2
                                                  id (@. this model (get "handId")))))
                    nil))
     (refine-waist (lambda ()
                     (@. this model collection 
                         parent-model (get "vent")
                         (trigger "refine" (create title (@. this model (get "waist"))
                                                   part-id 3
                                                   id (@. this model (get "waistId")))))
                     nil))
     (refine-foot (lambda ()
                    (@. this model collection 
                        parent-model (get "vent")
                        (trigger "refine" (create title (@. this model (get "foot"))
                                                  part-id 4
                                                  id (@. this model (get "footId")))))
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
                  per-page 24
                  total-entries 0
                  remain 0
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
     (initialize (lazy-init
                  (setf (@ this init-push) true)
                  ((@ ($ (@ this parent-node)) append) (@ ((@ this render)) el))
                  ((@ this model list each) (@ this lazy-add))
                  (when (> (@ this model list length) 0)
                    (@. this ($ ".cascade") (masonry (create item-selector ".item"
                                                             gutter 10))))
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
               this))))


(def-view load-more-button
    ((tag-name "div")
     (template (tmpl-from "load-more.tmpl"))
     (events (create "click .continue" "continue"))
     (initialize (lazy-init
                  ((@ ($ (@ this parent-node)) append) (@ ((@ this render)) el))
                  (@. this (listen-to (@ this model)
                                      "change"
                                      (@ this render)))
                  (@. this (listen-to (@ this model list)
                                      "add"
                                      (@ this render)))
                  nil))
     (render (lambda ()
               (@. this model (set "remain"
                                   (- (@. this model (get "totalEntries"))
                                      (@ this model list length))))
               (render-from-model)
               (when (not (and (> (@ this model list length) 0)
                               (> (@. this model (get "remain")) 0)))
                 (@. this ($ ".continue") (add-class "disabled")))
               this))
     (continue (lambda ()
                 (if (and (> (@ this model list length) 0)
                          (> (@. this model (get "remain")) 0))
                     (@. this model (get "vent")
                         (trigger "getpage"
                                  (create sets (@ this model)))))
                 nil))))












