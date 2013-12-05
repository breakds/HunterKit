;;;; app.lisp
;;;; in hunter-kit

(in-package #:hunter-kit)


(defun group (lst n)
  "group elements of a list into n-sized sub-lists"
  (if (zerop n) 
      (error "group size is zero.")
      (labels ((rec (rest accu)
                 (if (null rest)
                     (nreverse accu)
                     (rec (nthcdr n rest)
                          (cons (if (>= (length rest) n)
                                    (subseq rest 0 n)
                                    rest)
                                accu)))))
        (rec lst nil))))



;;; code generator

(defun make-skill-group (skill-item)
  `(progn (setf tmp-sk 
                (duplicate skill-group
                           :name ,(skill-name skill-item)
                           :skill-id ,(skill-id skill-item)
                           :vent (@ this vent)))
          ,@(mapcar (lambda (x) `((@ tmp-sk list add) (create name ,(cadr x)
                                                              description ,(caddr x)
                                                              points ,(car x))))
                    (remove-if (lambda (x) (< (car x) 0)) (skill-tags skill-item)))
          ((@ tmp-sk list add) (create name "off" points 0))))


(defun make-skill-group-row (skills)
  `(progn (setf tmp-array (make-array))
          ,@(loop for skill-item in skills
               collect `(progn
                          ,(make-skill-group skill-item)
                          ((@ tmp-array push) tmp-sk)))
          (setf tmp-row 
                (duplicate skill-group-row-model
                           :model-list tmp-array))))


(defun make-skill-group-all (skills)
  `(progn (setf tmp-rows (make-array))
          ,@(loop for every in (group skills 6)
               collect `(progn
                          ,(make-skill-group-row every)
                          ((@ tmp-rows push) tmp-row)))
          (setf (@ this skill-rows) 
                (duplicate skill-set-model
                           :model-list tmp-rows))))

(defun make-stone-slot (name label skills)
  `(progn (setf tmp-array (make-array))
          ((@ tmp-array push) 
           (duplicate single-skill-option-model
                      :name "None"
                      :skill-id -1))
          ,@(loop for skill-item in *skills*
               collect `((@ tmp-array push)
                         (duplicate single-skill-option-model
                                    :name ,(skill-name skill-item)
                                    :skill-id ,(skill-id skill-item))))
          (setf (@ this ,name)
                (duplicate skill-options
                           :label ,label
                           :selected-id -1
                           :model-list tmp-array))))


(init) ;; initialize the system

(def-router web-app-router
    ((initialize (lambda (args)

                   ;; prepare event
                   (create-event-manager (@ this vent))
                   
                   ((@ this vent on) "adjust"
                    (lambda (args)
                      (let ((skill-id (@ args id))
                            (points (@ args points))
                            (caption (@ args caption))
                            (description (@ args description))
                            (active-skills (@ this active-skills list)))
                        (let ((obj ((@ active-skills get) skill-id)))
                          (if obj
                              (if (equal points 0)
                                  ((@ active-skills remove) obj)
                                  (when (not (equal points ((@ obj get) "points")))
                                    ((@ active-skills remove) obj)
                                    ((@ active-skills add) (duplicate single-active-model
                                                                      :id skill-id
                                                                      :points points
                                                                      :caption caption
                                                                      :description description))))
                              (if (> points 0)
                                  ((@ active-skills add) (duplicate single-active-model
                                                                    :id skill-id
                                                                    :points points
                                                                    :caption caption
                                                                    :description description)))))))
                    this)

                   
                   ;; passively tab switch
                   ((@ this vent on) "toresult"
                    (lambda (args)
                      ((@ this navigate) "result" (create trigger true)))
                    this)

                   ((@ this vent on) "refreshResult"
                    (lambda (args)
                      (@. this (resulting)))
                    this)
                   
                   ;; armor set list
                   (setf (@ this result-list)
                         (duplicate armor-sets 
                                    :url "/hunterkit/search"
                                    vent (@ this vent)))


                   ;; armor black list
                   (eval-lisp 
                    `(let ((tmp-array (make-array)))
                       ,@(loop 
                            for part-set across *armor-set*
                            for part-id from 0
                            collect `(let ((tmp-array-1 (make-array)))
                                       ,@(loop 
                                            for armor-item in part-set
                                            for id from 0
                                            collect `((@ tmp-array-1 push)
                                                      (duplicate single-colored-armor-model
                                                                 :id ,id
                                                                 :part-id ,part-id
                                                                 :caption ,(armor-name armor-item))))
                                       ((@ tmp-array push)
                                        (duplicate colored-armor-sublist
                                                   :label ,(cond 
                                                            ((= part-id 0) "头部防具")
                                                            ((= part-id 1) "胸部护甲")
                                                            ((= part-id 2) "腰带")
                                                            ((= part-id 3) "手套")
                                                            ((= part-id 4) "足部护具"))
                                                   :model-list tmp-array-1))))
                       (setf (@ this armor-select-list)
                             (duplicate colored-armor-list
                                        :black (array "0,230" "0,231" "0,232" "0,233" "0,234" 
                                                      "0,235" "0,236" "0,237" "0,238" "0,239" 
                                                      "0,240" "0,256"
                                                      "0,241" "0,242" "0,243" "0,244" "0,245" 
                                                      "0,246" "0,247" "0,248" "0,249" "0,250" 
                                                      "0,251" "0,252" "0,253" "0,254" "0,255")
                                        :model-list tmp-array))))

                   
                   
                   ((@ this vent on) "dosearch"
                    (lambda (args)
                      (setf (@ this result-list list url) "/hunterkit/search")
                      (@. this loading-splash (modal "Calculating ..."))
                      ((@ this result-list list fetch)
                       (create 
                        type "post"
                        data ((@ *json* stringify) (create 
                                                    per-page ((@ this result-list get) "perPage")
                                                    weapon (@. this search-btn-model (get "weapon"))
                                                    black-list (@. this armor-select-list 
                                                                   (get "black"))
                                                    req ((@ this active-skills list collect) 
                                                         (lambda (x)
                                                           (create id ((@ x get) "id")
                                                                   points ((@ x get) "points"))))
                                                    stone-a (create id (*Number (@. this stone-slot-a
                                                                                    (get "selectedId")))
                                                                    points (*Number (@. this stone-slot-a
                                                                                        (get "selectedVal"))))
                                                    stone-b (create id (*Number (@. this stone-slot-b
                                                                                    (get "selectedId")))
                                                                    points (*Number (@. this stone-slot-b
                                                                                        (get "selectedVal"))))))
                        success (lambda (collection response options)
                                  (setf (@ collection parent-model url) "/hunterkit/meta")
                                  ((@ collection parent-model fetch)
                                   (create
                                    type "post"
                                    success (lambda (model response options)
                                              ((@ model set) "perPage" (*number ((@ model get) "perPage")))
                                              ((@ model set) "totalEntries" 
                                               (*number ((@ model get) "totalEntries")))
                                              (@. model (get "vent") (trigger
                                                                      "toresult"
                                                                      (create)))
                                              (@. model (get "vent") (trigger "wait-finish")))))))))
                    this)
                   
                   ((@ this vent on) "getpage"
                    (lambda (args)
                      (setf (@ this result-list list url) "/hunterkit/page")
                      ((@ this result-list list fetch)
                       (create 
                        type "post"
                        remove false)))
                    this)

                   ((@ this vent on) "wait-finish"
                    (lambda (args)
                      (@. this loading-splash (hide))
                      nil)
                    this)

                   ((@ this vent on) "refine"
                    (lambda (args)
                      (@. this refiner (modal (@ args title)
                                              (@ args part-id)
                                              (@ args id)))
                      nil)
                    this)
                   
                   ((@ this vent on) "dorefine"
                    (lambda (args)
                      (setf (@ this result-list list url) "/hunterkit/refine")
                      (@. this loading-splash (modal "Refining Results ..."))
                      ((@ this result-list list fetch)
                       (create 
                        type "post"
                        data ((@ *json* stringify) (create 
                                                    operation (@ args op)
                                                    per-page ((@ this result-list get) "perPage")
                                                    part-id (*number (@ args part-id))
                                                    id (*number (@ args id))))
                        success (lambda (collection response options)
                                  (setf (@ collection parent-model url) "/hunterkit/meta")
                                  ((@ collection parent-model fetch)
                                   (create
                                    type "post"
                                    success (lambda (model response options)
                                              ((@ model set) "perPage" (*number ((@ model get) "perPage")))
                                              ((@ model set) "totalEntries" 
                                               (*number ((@ model get) "totalEntries")))
                                              (@. model (get "vent") (trigger
                                                                      "refreshResult"
                                                                      (create)))
                                              (@. model (get "vent") (trigger "wait-finish")))))))))
                    this)

                   
                   
                   
                   ;; app status variables 
                   (setf (@ this page) undefined)
                   
                   ;; prepare skill menu
                   (eval-lisp (make-skill-group-all *skills*))


                   ;; prepare active list
                   (setf (@ this active-skills)
                         (duplicate active-list-model
                                    :model-list (make-array)))

                   ;; prepare stone slots
                   (eval-lisp (make-stone-slot 'stone-slot-a "护石技能A" 
                                               *skills*))
                   (eval-lisp (make-stone-slot 'stone-slot-b "护石技能B" 
                                               *skills*))
                   
                   ;; prepare search button
                   (setf (@ this search-btn-model) (duplicate search-button-model
                                                              :caption "Search"
                                                              :vent (@ this vent)))
                   
                   ;; deploy navigation bar
                   (setf (@ this navigation)
                         (duplicate navigation
                                    :model (duplicate tab-collection)
                                    :parent-node ($ "#navigation")))
                   

                   ;; loading page
                   (setf (@ this loading-splash)
                         (duplicate loading-page
                                    :model (duplicate loading-page-model
                                                      :caption "loading...")
                                    :parent-node ($ "#loading")))


                   ;; refine dialog
                   (setf (@ this refiner)
                         (duplicate refine-dialog
                                    :model (duplicate refine-dialog-model
                                                      :title "abc"
                                                      :vent (@ this vent))
                                    :parent-node ($ "#refining")))

                   ;; talking board
                   (setf (@ this about-model)
                         (duplicate talking-board-model
                                    :router this))
                   
                   
                   ((@ this navigation add) (create tab-name "search" tab-title "Search" id 0))
                   ((@ this navigation add) (create tab-name "result" tab-title "Result" id 1))
                   ((@ this navigation add) (create tab-name "config" tab-title "Config" id 2))
                   ((@ this navigation add) (create tab-name "about" tab-title "About" id 3))
                   nil))
     (routes (create 
              "search" "searching"
              "result" "resulting"
              "config" "configuring"
              "about" "talking"))
     (searching (lambda ()
                  ((@ this navigation model switch-to) 
                   (@. this navigation model list (get 0) cid))
                  (when (not (equal undefined (@ this page)))
                    ((@ this page terminate)))
                  (setf (@ this page) 
                        (duplicate page
                                   :parent-node ($ "#content")))
                  ((@ this page append-view) skill-set (create model (@ this skill-rows)))
                  (setf right-sub-page
                        (duplicate page
                                   :parent-node (@ this page el)
                                   :additional-classes "span3"))
                  
                  ((@ this page add-sub-view) right-sub-page)
                  ((@ right-sub-page append-view) active-list (create model (@ this active-skills)))
                  ((@ right-sub-page append-view) stone-skill-select-view 
                   (create model (@ this stone-slot-a)))
                  ((@ right-sub-page append-view) stone-skill-select-view 
                   (create model (@ this stone-slot-b)))
                  ((@ right-sub-page append-view) search-button (create model (@ this search-btn-model)))
                  nil))
     (resulting (lambda ()
                  ((@ this navigation model switch-to) 
                   (@. this navigation model list (get 1) cid))
                  (when (not (equal undefined (@ this page)))
                    ((@ this page terminate)))
                  (setf (@ this page) 
                        (duplicate page
                                   :parent-node ($ "#content")))
                  ((@ this page append-view) armor-sets-table (create model (@ this result-list)))
                  ((@ this page append-view) load-more-button (create model (@ this result-list)))
                  nil))
     (configuring (lambda ()
                    (@. this loading-splash (modal "loading black list ..."))
                    ((@ this navigation model switch-to)
                     (@. this navigation model list (get 2) cid))
                    (when (not (equal undefined (@ this page)))
                      ((@ this page terminate)))
                    (setf (@ this page)
                          (duplicate page
                                     :parent-node ($ "#content")))
                    ((@ this page append-view) armor-select
                     (create model (@ this armor-select-list)))
                    (@. this loading-splash (hide))))
     (talking (lambda ()
                ((@ this navigation model switch-to) 
                 (@. this navigation model list (get 3) cid))
                (when (not (equal undefined (@ this page)))
                  ((@ this page terminate)))
                (setf (@ this page) 
                      (duplicate page
                                 :parent-node ($ "#content")))
                ((@ this page append-view) talking-board (create model (@ this about-model)))
                nil))))












(define-simple-app hunter-kit-app
    (:title "Hunter Kit v0.9 RC" 
            :uri "/hunterkit"
            :port 9701
            :document-base (merge-pathnames "assets/" (asdf:system-source-directory 'hunter-kit))
            :template (merge-pathnames "assets/main.tmpl" (asdf:system-source-directory 'hunter-kit))
            :css ("http://cdnjs.cloudflare.com/ajax/libs/twitter-bootstrap/2.3.2/css/bootstrap.css"
                  "http://cdnjs.cloudflare.com/ajax/libs/twitter-bootstrap/2.3.2/css/bootstrap-responsive.css"
                  "http://cdnjs.cloudflare.com/ajax/libs/select2/3.4.0/select2.min.css"
                  "/hunterkit/css/tiled.css")
            :libs (;; JQuery from Google Ajax cdn
		   "http://ajax.googleapis.com/ajax/libs/jquery/1.10.1/jquery.min.js"
		   ;; underscore.js from cdnjs
		   "http://cdnjs.cloudflare.com/ajax/libs/underscore.js/1.4.4/underscore-min.js"
		   ;; backbone.js from cdnjs
                   "http://cdnjs.cloudflare.com/ajax/libs/backbone.js/1.0.0/backbone-min.js"
                   ;; bootstrap
                   "http://cdnjs.cloudflare.com/ajax/libs/twitter-bootstrap/2.3.2/js/bootstrap.min.js"
                   ;; masonry
                   "http://cdnjs.cloudflare.com/ajax/libs/masonry/3.0.0/masonry.pkgd.min.js"
                   ;; selecte
                   "http://cdnjs.cloudflare.com/ajax/libs/select2/3.4.0/select2.min.js"))
  (defvar app (new (web-app-router)))
  ((@ *backbone history start))
  ((@ app navigate) "about" true))


