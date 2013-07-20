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
               this))
     (modal (lambda (caption)
              (@. this model (set "caption" caption))
              (@. this $el (modal))
              this))
     (hide (lambda ()
             (@. this $el (modal "hide"))))))
                    
     
    