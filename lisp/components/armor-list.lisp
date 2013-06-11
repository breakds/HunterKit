;;;; armor-list.lisp
;;;; widget: armor list 

(in-package #:breakds.hunter-kit)

;; todo color and rare

(def-model single-armor-set-model
    (('defaults '(lambda ()
                  (create 
                   head ""
                   head-jewel "灵猫珠 萌神珠"
                   chest ""
                   chest-jewel ""
                   hand ""
                   hand-jewel ""
                   waist ""
                   waist-jewel ""
                   foot ""
                   foot-jewel "")))))

(def-view single-armor-set
    (('tag-name "tr")
     ('template #.(read-tmpl "single-armor-set.tmpl"))
     ('initialize '(lazy-init
                    ((@ ($ (@ this parent-node)) append) (@ ((@ this render)) el))
                    nil))
     ('render '(lambda ()
                (render-from-model)
                (@. this ($ "jewels") (tooltip (create animation true placement "top")))
                this))))

(def-collection armor-sets
    (('defaults '(lambda ()
                  (create 
                   page 0
                   total 0
                   max-entries 20)))
     ('initialize '(lazy-init
                    (setf (@ this list url) (@ args url))
                    nil))
     ('model 'single-armor-set-model)))

(def-collection-view armor-sets-table
    (('tag-name "table")
     ('template #.(read-tmpl "armor-sets-table.tmpl"))
     ('sub-view 'single-armor-set)
     ('initialize '(lazy-init
                    ((@ ($ (@ this parent-node)) append) (@ ((@ this render)) el))
                    ((@ this model list each) (@ this lazy-add))
                    nil))
     ('render '(lambda ()
                (render-from-model)
                ((@ this $el add-class) "table table-hover")
                this))))



                   
    
     
                   
     
    