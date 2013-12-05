;;;; stone-slot.lisp
;;;; widget: select for stone skills

(in-package #:breakds.hunter-kit)


(def-model single-skill-option-model
    ((initialize (lambda (args)
                   (acquire-args
                    (name skill-id)
                    (name skill-id))
                   nil))))

(def-view single-skill-option
    ((tag-name "option")
     (template "<%=name%>")
     (initialize (lazy-init
                  ((@ ($ (@ this parent-node)) append) (@ ((@ this render)) el))
                  nil))
     (render (lambda ()
               (render-from-model)
               (@. this $el (attr "value" (@. this model (get "skillId"))))
               this))))

(def-collection skill-options
    ((defaults (lambda () (create label ""
                                  selected-id -1
                                  selected-val 0)))
     (model single-skill-option-model)))

(def-collection-view stone-skill-select-view
    ((tag-name "div")
     (events (create "change div .stone-skill" "skillSwitch"
                     "change div .stone-point" "valueSwitch"))
     (sub-view single-skill-option)
     (template (tmpl-from "stone-slot.tmpl"))
     (initialize (lazy-init
                  ((@ ($ (@ this parent-node)) append) (@ ((@ this render)) el))
                  ((@ this model list each) (@ this lazy-add))
                  (@. this ($ "div .stone-skill")
                      (val (@. this model (get "selectedId"))))
                  (@. this ($ "div .stone-point")
                      (val (@. this model (get "selectedVal"))))
                  nil))
     (render (lambda ()
               (render-from-model)
               (@. this $el (add-class "form-group"))
               this))
     (skill-switch (lambda (e)
                     (@. this model 
                         (set "selectedId"
                              (@. this ($ "div .stone-skill") (val))))))
     (value-switch (lambda (e)
                     (@. this model 
                         (set "selectedVal"
                              (@. this ($ "div .stone-point") (val))))))
     (entry-point "div .stone-skill")))

