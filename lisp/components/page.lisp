;;;; page.lisp
;;;; widget: page containter

(in-package #:breakds.hunter-kit)

(def-view page
    (('tag-name "div")
     ('template "")
     ('initialize '(lazy-init
                    (if (@ args additional-classes)
                        (setf (@ this additional-classes) (@ args additional-classes))
                        (setf (@ this additional-classes) ""))
                    (setf (@ this model) (new ((@ *backbone *model))))
                    (@. ($ (@ this parent-node)) (append (@. this (render) el)))
                    nil))
     ('render '(lambda ()
                (render-from-model)
                ((@ this $el add-class) "row-fluid")
                ((@ this $el add-class) (@ this additional-classes))
                this))
     ('append-view '(lambda (view-class args)
                     ((@ this add-sub-view) 
                      (new (view-class 
                            ((@ _ extend) 
                             args 
                             (create parent-node (@ this $el))))))))))




                    