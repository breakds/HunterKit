;;;; refine-dialog.lisp
;;;; widget: the dialog for refining options

(in-package #:breakds.hunter-kit)

(def-model refine-dialog-model
    ((defaults (lambda () (create title ""
                                  part-id 0
                                  id 0)))))

(def-view refine-dialog
    ((tag-name "div")
     (template (tmpl-from "refine-dialog.tmpl"))
     (initialize (lazy-init
                  ((@ ($ (@ this parent-node)) append) (@ ((@ this render)) el))
                  (@. this (listen-to
                            (@ this model)
                            "change"
                            (@ this render)))
                  nil))
     (events (create "click .btn-pin" "pinArmor"
                     "click .btn-remove" "dropArmor"))
     (pin-armor (lambda ()
                  (@. this (hide))
                  (@. this model (get "vent")
                      (trigger "dorefine"
                               (create part-id (@. this model (get "partId"))
                                       id (@. this model (get "id"))
                                       op "pin")))
                  nil))
     (drop-armor (lambda ()
                   (@. this (hide))
                   (@. this model (get "vent") 
                       (trigger "dorefine"
                                (create part-id (@. this model (get "partId"))
                                        id (@. this model (get "id"))
                                        op "drop")))
                   nil))
     (render (lambda ()
               (render-from-model)
               ((@ this $el add-class) "modal hide fade")
               this))
     (modal (lambda (title part-id id)
              (@. this model (set "title" title))
              (@. this model (set "partId" part-id))
              (@. this model (set "id" id))
              (@. this $el (modal))
              nil))
     (hide (lambda ()
             (@. this $el (modal "hide"))))))
               
                 