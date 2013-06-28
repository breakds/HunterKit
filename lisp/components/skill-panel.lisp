;;;; skill-panel.lisp
;;;; widget: drop down button groups for skills


(in-package #:breakds.hunter-kit)


(def-model single-skill-model
    ((initialize (lambda (args)
                    (acquire-args
                     (name points description)
                     (name points description))
                    nil))))

(def-view single-skill
    ((tag-name "li")
     (template "<a href=\"#\"><%=name%></a>")
     (events (create "click a" "onClick"))
     (initialize (lazy-init 
                    ((@ ($ (@ this parent-node)) append) (@ ((@ this render)) el))
                    nil))
     (render (lambda ()
                (render-from-model)
                this))
     (on-click (lambda ()
                  ((@ this model trigger)
                   "turned"
                   (@ this model cid))
                  nil))))

(def-collection skill-group
    ((defaults (lambda () (create 
                             points 0
                             skill-id -1
                             name ""
                             caption "")))
     (initialize (lazy-init
                    ((@ this set) "caption" (@ args name))
                    nil))
     (turn-on (lambda (cid)
                 (let ((pts (@. this list (get cid) (get "points")))
                       (caption (@. this list (get cid) (get "name")))
                       (description (@. this list (get cid) (get "description"))))
                   (if (= 0 pts)
                       ((@ this set) "caption" ((@ this get) "name"))
                       ((@ this set) "caption" (+ (@. this list (get cid) (get "name")) " (" pts ")")))
                   ((@ this set) "points" pts)
                   (@. this (get "vent") (trigger 
                                          "adjust"
                                          (create 
                                           id ((@ this get) "skillId") 
                                           points pts
                                           description description
                                           caption caption))))))
     (model single-skill-model)))


(def-collection-view skill-button
    ((tag-name "div")
     (template (tmpl-from "skill-button.tmpl"))
     (sub-view single-skill)
     (initialize (lazy-init
                    ((@ this listen-to)
                     (@ this model list)
                     "turned"
                     (lambda (cid)
                       ((@ this model turn-on) cid)
                       nil))
                    ((@ this listen-to)
                     (@ this model)
                     "change"
                     (@ this render))
                    ((@ this model list each) (@ this lazy-add))
                    ((@ ($ (@ this parent-node)) append) (@ ((@ this render)) el))
                    nil))
     (render (lambda ()
                (render-from-model)
                ((@ this $el add-class) "dropdown span2")
                (if (= ((@ this model get) "points") 0)
                    (@. this ($ "button") (remove-class "btn-primary"))
                    (@. this ($ "button") (add-class "btn-primary")))
                ((@ this lazy-render-sub-views))
                this))
     (add (lambda (args)
             ((@ this model list add) args)))
     (entry-point ".dropdown-menu")))                     

(def-collection skill-group-row-model
    ((model skill-group)))

(def-collection-view skill-button-row
    ((tag-name "div")
     (template "")
     (sub-view skill-button)
     (initialize (lazy-init
                    ((@ ($ (@ this parent-node)) append) (@ ((@ this render)) el))
                    ((@ this model list each) (@ this lazy-add))
                    nil))
     (render (lambda ()
                (render-from-model)
                ((@ this $el add-class) "row-fluid")
                this))))

(def-collection skill-set-model
    ((model skill-group-row-model)))

(def-collection-view skill-set
    ((tag-name "div")
     (template "")
     (sub-view skill-button-row)
     (initialize (lazy-init
                    ((@ ($ (@ this parent-node)) append) (@ ((@ this render)) el))
                    ((@ this model list each) (@ this lazy-add))
                    nil))
     (render (lambda ()
                (render-from-model)
                ((@ this $el add-class) "span9")
                this))))

     