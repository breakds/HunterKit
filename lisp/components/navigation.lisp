;;;; navigation.lisp
;;;; widget: navigation tabs


(in-package #:breakds.hunter-kit)

(def-model single-tab-model
    (('initialize '(lambda (args)
                    (acquire-args
                     (tab-name tab-title)
                     (tab-name tab-title))
                    (setf (@ this active) false)
                    nil))))

(def-view single-tab
    (('tag-name "li")
     ('template "<a href=\"#<%=tabName%>\"><%=tabTitle%></a>")
     ('events '(create "click a" "onClick"))
     ('initialize '(lazy-init
                    ((@ this listen-to)
                     (@ this model)
                     "change"
                     (@ this render))
                    ((@ ($ (@ this parent-node)) append) (@ ((@ this render)) el))))
     ('render '(lambda ()
                (render-from-model)
                (if ((@ this model get) "active")
                    ((@ this $el add-class) "active")
                    ((@ this $el remove-class) "active"))
                this))
     ('on-click '(lambda ()
                  ((@ this model trigger) 
                   "switch" 
                   (@ this model cid))
                  nil))))


(def-collection tab-collection
    (('defaults '(lambda () (create active-tab undefined)))
     ('switch-to '(lambda (cid)
                   (let ((active-tab ((@ this get) "activeTab")))
                     (when (not (equal cid active-tab))
                       (when (not (equal undefined active-tab))
                         ((@ ((@ this list get) active-tab) set) "active" false))
                       ((@ this set) "activeTab" cid)
                       ((@ ((@ this list get) cid) set)
                        "active" true)))
                   nil))
     ('model 'single-tab-model)))

(def-collection-view navigation
    (('tag-name "ul")
     ('sub-view 'single-tab)
     ('initialize '(lazy-init
                    ((@ this listen-to)
                     (@ this model list)
                     "switch"
                     (lambda (cid)
                       ((@ this model switch-to) cid)
                       nil))
                    ((@ ($ (@ this parent-node)) append) (@ ((@ this render)) el))
                    nil))
     ('render '(lambda ()
                ((@ this $el empty))
                ((@ this $el add-class) "nav nav-tabs")
                this))
     ('add '(lambda (args)
             ((@ this model list add) args)))))
                   
