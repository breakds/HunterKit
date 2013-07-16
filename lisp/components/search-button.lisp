;;;; search-button.lisp
;;;; widget: button for search


(in-package #:breakds.hunter-kit)

(def-model search-button-model
    ((defaults (lambda () (create caption "Search"
                                  weapon "saber")))))


(def-view search-button
    ((tag-name "form")
     (template (tmpl-from "submit-form.tmpl"))
     (events (create "click button" "onClick"
                     "click .option-saber" "tosaber"
                     "click .option-archer" "toarcher"))
     (initialize (lazy-init
                    ((@ ($ (@ this parent-node)) append) (@ ((@ this render)) el))
                    nil))
     (render (lambda ()
                (render-from-model)
                ((@ this $el add-class) "form-inline")
                (@. this ($ (+ ".option-" 
                               (@. this model (get "weapon"))))
                    (attr "checked" "true"))
                this))
     (on-click (lambda ()
                 (@. this model (get "vent") 
                     (trigger "dosearch" (create weapon 
                                                 (@. this 
                                                     model 
                                                     (get "weapon")))))
                 nil))
     (tosaber (lambda ()
                (@. this model (set "weapon" "saber"))))
     (toarcher (lambda ()
                 (@. this model (set "weapon" "archer"))))))
                

                                   