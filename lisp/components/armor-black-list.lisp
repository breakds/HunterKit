;;;; armor-black-list.lisp
;;;; widget: swappable black list

(in-package #:breakds.hunter-kit)

(def-model single-colored-armor-model
    ((defaults (lambda ()
                 (create id 0
                         part-id 0
                         caption "")))))

(def-view single-armor-entry
    ((tag-name "option")
     (template "<%=caption%>")
     (initialize (lazy-init
                  ((@ ($ (@ this parent-node)) append) (@ ((@ this render)) el))
                  nil))
     (render (lambda ()
               (render-from-model)
               (@. this $el (attr "value" (@. this model (get "id"))))
               this))))


(def-collection colored-armor-sublist
    ((defaults (lambda () (create label "")))
     (model single-colored-armor-model)))

(def-collection-view colored-armor-optgroup
    ((tag-name "optgroup")
     (sub-view single-armor-entry)
     (initialize (lazy-init
                  ((@ ($ (@ this parent-node)) append) (@ ((@ this render)) el))
                  ((@ this model list each) (@ this lazy-add))
                  nil))
     (render (lambda ()
               (render-from-model)
               (@. this $el (attr "label" (@. this model (get "label"))))
               this))))


(def-collection colored-armor-list
    ((model colored-armor-sublist)))


(def-collection-view armor-select
    ((tag-name "div")
     (events (create "change select" "report"))
     (sub-view colored-armor-optgroup)
     (template (tmpl-from "armor-select.tmpl"))
     (initialize (lazy-init
                  ((@ ($ (@ this parent-node)) append) (@ ((@ this render)) el))
                  ((@ this model list each) (@ this lazy-add))
                  nil))
     (render (lambda ()
               (render-from-model)
               (@. this ($ "select") (select2 (create placeholder "add armors to blacklist ...")))
               this))
     (report (lambda (e)
               (trace (@ e val))))
     (entry-point "select")))


     
                 
                           
