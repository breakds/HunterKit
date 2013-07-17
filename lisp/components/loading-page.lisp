;;;; loading-page.lisp
;;;; widget: the loading page

(in-package #:breakds.hunter-kit)

(def-model loading-page-model
    ((defaults (lambda () (create caption "")))))

(def-view loading-page
    ((tag-name "div")
     (template (tmpl-from "loading-page.tmpl"))
     (initialize (lazy-init
                  ((@ ($ (@ this parent-node)) append) (@ ((@ this render)) el))
                  (@. this (listen-to
                            (@ this model)
                            "change"
                            (@ this render)))
                  nil))
     (render (lambda ()
               (render-from-model)
               ((@ this $el add-class) "modal hide fade")
               (@. this $el (modal (create backdrop "static"
                                           show false)))
               (@. this ($ ".bar") (css "font-size" "20px"))
               (@. this ($ ".bar") (css "width" "10em"))
               (@. this ($ ".bar") (css "height" "1em"))
               (@. this ($ ".bar") (css "position" "relative"))
               (@. this ($ ".bar") (css "margin" "100px auto"))
               (@. this ($ ".bar") (css "border-radius" ".5em"))
               (@. this ($ ".bar") (css "background" "rgba(255,255,255,0.6)"))
               

               (@. this ($ ".sphere") (css "display" "block"))
               (@. this ($ ".sphere") (css "width" "1em"))
               (@. this ($ ".sphere") (css "height" "100%"))
               (@. this ($ ".sphere") (css "border-radius" "50%"))
               (@. this ($ ".sphere") (css "animation" "move 1.75s ease-in-out infinite alternate"))
               
               this))
     (modal (lambda (caption)
              (@. this model (set "caption" caption))
              (@. this $el (modal))
              this))
     (hide (lambda (caption)
             (@. this $el (modal "hide"))))))
                    
     
    