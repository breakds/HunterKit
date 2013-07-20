;;;; talking-board.lisp
;;;; widget: for "about"

(in-package #:breakds.hunter-kit)

(def-model talking-board-model
    ((defaults (lambda () (create router undefined)))))

(def-view talking-board
    ((tag-name "div")
     (template (tmpl-from "talking-board.tmpl"))
     (events (create "click .btn-start" "startApp"))
     (start-app (lambda ()
                  (@. this model (get "router") (navigate "search" true))
                  nil))
     (initialize (lazy-init
                  ((@ ($ (@ this parent-node)) append) (@ ((@ this render)) el))
                  nil))
     (render (lambda ()
               (render-from-model)
               this))))
     


