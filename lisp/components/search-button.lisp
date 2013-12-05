;;;; search-button.lisp
;;;; widget: button for search


(in-package #:breakds.hunter-kit)


(def-model search-button-model
    ((defaults (lambda () (create caption "Search"
                                  weapon "saber")))))

(def-view search-button
    ((tag-name "div")
     (template (tmpl-from "submit-form.tmpl"))
     (events (create "click button" "onClick"
                     "click .option-saber" "tosaber"
                     "click .option-archer" "toarcher"))
     (initialize (lazy-init
                  ((@ ($ (@ this parent-node)) append) (@ ((@ this render)) el))
                  (@. this (listen-to (@ this model)
                                      "change"
                                      (@ this render)))
                  nil))
     (render (lambda ()
               (render-from-model)
               (@. this ($ (+ ".option-" 
                              (@. this model (get "weapon"))))
                   (add-class "active"))
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


