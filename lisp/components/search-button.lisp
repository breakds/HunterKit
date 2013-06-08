;;;; search-button.lisp
;;;; widget: button for search


(in-package #:breakds.hunter-kit)

(def-model search-button-model
    (('defaults (lambda () (create caption "Search")))))


(def-view search-button
    (('tag-name "button")
     ('template "<%=caption%>")
     ('events '(create "click" "onClick"))
     ('initialize '(lazy-init
                    ((@ ($ (@ this parent-node)) append) (@ ((@ this render)) el))
                    nil))
     ('render '(lambda ()
                (render-from-model)
                ((@ this $el add-class) "btn btn-info")
                this))
     ('on-click '(lambda ()
                  (@. this model (get "vent") 
                   (trigger "dosearch" (create)))
                  nil))))

                                   