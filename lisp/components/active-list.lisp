;;;; active-list.lisp
;;;; widget: a list of active skills


(in-package #:breakds.hunter-kit)


(def-model single-active-model
    ((defaults (lambda () (create 
                           points 0
                           caption ""
                           description "")))))

(def-view single-active
    ((tag-name "li")
     (template (tmpl-from "single-active.tmpl"))
     (initialize (lazy-init
                  ((@ ($ (@ this parent-node)) append) (@ ((@ this render)) el))
                  nil))
     (render (lambda ()
               (render-from-model)
               this))))



(def-collection active-list-model
    ((model single-active-model)))

(def-collection-view active-list
    ((tag-name "ul")
     (sub-view single-active)
     (initialize (lazy-init
                  ((@ ($ (@ this parent-node)) append) (@ ((@ this render)) el))
                  ((@ this model list each) (@ this lazy-add))
                  nil))
     (render (lambda ()
               (render-from-model)
               ((@ this $el add-class) "media-list")
               this))))

                             

                             