;;;; armor-black-list.lisp
;;;; widget: swappable black list

(in-package #:breakds.hunter-kit)

(def-model single-colored-armor-model
    ((defaults (lambda ()
                 (create part-id 0
                         id 0
                         name ""
                         white 1)))))


(def-view single-armor-entry
    ((tag-name "option")
     (template "<%=name%>")
     (initialize (lazy-init
                  ((@ ($ (@ this parent-node)) append) (@ ((@ this render)) el))
                  nil))
     (render (lambda ()
               (render-from-model)
               (@. this $el (attr "data-icon" "icon-ok"))
               this))))


(def-collection colored-armor-list
    ((model single-colored-armor-model)))


(def-collection-view armor-black-list
    ((tag-name "div")
     (template (tmpl-from "armor-black-list.tmpl"))
     (sub-view single-armor-entry)
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
               this))
     (entry-point "select")))


(def-view armor-select
    ((tag-name "div")
     (events (create "change select" "report"))
     (template (tmpl-from "armor-select.tmpl"))
     (initialize (lazy-init
                  ((@ ($ (@ this parent-node)) append) (@ ((@ this render)) el))
                  nil))
     (render (lambda ()
               (render-from-model)
               (@. this ($ "select") (select2 (create placeholder "add armors to blacklist ...")))
               this))
     (report (lambda (e)
               (trace (@ e val))))))


     
                 
                           
