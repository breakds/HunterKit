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
  `(progn (setf tmp-sk (new (skill-group (create name ,(skill-name skill-item)
                                                 skill-id ,(skill-id skill-item)
                                                 vent (@ this vent)))))
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
          (setf tmp-row (new (skill-group-row-model
                              (create model-list tmp-array))))))

(defun make-skill-group-all (skills)
  `(progn (setf tmp-rows (make-array))
          ,@(loop for every in (group skills 6)
               collect `(progn
                          ,(make-skill-group-row every)
                          ((@ tmp-rows push) tmp-row)))
          (setf (@ this skill-rows) (new (skill-set-model
                                          (create model-list tmp-rows))))))








(init) ;; initialize the system

(def-model toy-model
    (('defaults '(lambda () (create a 1 b 2)))
     ('url "/testa")))

(def-collection toy-collection
    (('model 'toy-model)))

(def-router web-app-router
    (('initialize `(lambda (args)

                     ;; prepare event
                     (setf (@ this vent) ((@ _ extend) (create) (@ *backbone *events)))
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
                                      ((@ active-skills add) (new (single-active-model
                                                               (create id skill-id
                                                                       caption caption
                                                                       description description))))))
                                
                                (if (> points 0)
                                    ((@ active-skills add) (new (single-active-model
                                                                 (create id skill-id
                                                                         caption caption
                                                                         description description)))))))))
                      this)

                     ;; test code

                     ;; (setf (@ this toy) (new (toy-model (create))))
                     ;; ((@ this vent on) "dosearch"
                     ;;  (lambda (args)
                     ;;    ((@ this toy fetch)
                     ;;     (create 
                     ;;      type "post"
                     ;;      data ((@ *json* stringify) (create val 12 the-data (create a 1 b 2)))
                     ;;      success (lambda (model response options)
                     ;;                       (trace response)
                     ;;                       (trace (+ "a: " ((@ model get) "a") " b: " ((@ model get) "b")))))))
                     ;;  this)

                     ;; (setf toy-a (new (toy-model (create a 1 b 2))))
                     ;; (setf toy-b (new (toy-model (create a 3 b 4))))
                     ;; (setf toy-c (new (toy-model (create a 5 b 6))))
                     
                     ;; (setf (@ this toys) (new (toy-collection
                     ;;                           (create 
                     ;;                            model-list (array toy-a toy-b toy-c)))))
                     ;; (setf (@ this toys list url) "/testa")
                     
                     ;; ((@ this vent on) "dosearch"
                     ;;  (lambda (args)
                     ;;    ((@ this toys list fetch)
                     ;;     (create 
                     ;;      type "post"
                     ;;      data ((@ *json* stringify) (create val 12 the-data (create a 1 b 2)))
                     ;;      success (lambda (collection response options)
                     ;;                ((@ collection each)
                     ;;                 (lambda (x)
                     ;;                   (trace (+ "a: " ((@ x get) "a") " b:" ((@ x get) "b")))))))))
                     ;;  this)
                        
                     
                     
                     ;; armor set list
                     (setf (@ this result-list) (new (armor-sets (create url "/testb"))))
                     ((@ this vent on) "dosearch"
                      (lambda (args)
                        ((@ this result-list list fetch)
                         (create 
                          type "post"
                          data ((@ *json* stringify) (create val 12))
                          success (lambda (collection response options)
                                    ((@ collection each)
                                     (lambda (x)
                                       (trace (+ "head: " ((@ x get) "head")))))))))
                      this)
                     
                     
                     ;; app status variables 
                     (setf (@ this page) undefined)
                     
                     ;; prepare skill menu
                     ,(make-skill-group-all *skills*)


                     ;; prepare active list
                     (setf (@ this active-skills) (new (active-list-model
                                                        (create model-list (make-array)))))

                     ;; prepare search button
                     (setf (@ this search-btn-model) (new (search-button-model 
                                                           (create caption "Search"
                                                                   vent (@ this vent)))))

                     
                     ;; deploy navigation bar
                     (setf (@ this navigation)
                           (new (navigation (create model (new (tab-collection (create)))
                                                    parent-node ($ "#navigation")))))
                     ((@ this navigation add) (create tab-name "search" tab-title "Search"))
                     ((@ this navigation add) (create tab-name "result" tab-title "Result"))
                     
                     nil))
     ('routes '(create 
                "search" "searching"
                "result" "resulting"))
     ('searching '(lambda ()
                   (when (not (equal undefined (@ this page)))
                     ((@ this page terminate)))
                   (setf (@ this page) (new (page
                                             (create parent-node
                                                     ($ "#content")))))
                   ((@ this page append-view) skill-set (create model (@ this skill-rows)))
                   (setf right-sub-page (new (page 
                                              (create parent-node
                                                      (@ this page el)
                                                      additional-classes
                                                      "span3"))))
                   ((@ this page add-sub-view) right-sub-page)
                   ((@ right-sub-page append-view) active-list (create model (@ this active-skills)))
                   ((@ right-sub-page append-view) search-button (create model (@ this search-btn-model)))
                   nil))
     ('resulting '(lambda ()
                   (when (not (equal undefined (@ this page)))
                     ((@ this page terminate)))
                   (setf (@ this page) (new (page
                                             (create parent-node
                                                     ($ "#content")))))
                   ((@ this page append-view) armor-sets-table (create model (@ this result-list)))))))






(define-simple-app hunter-kit-app
    (:title "The Hunter Kit" 
            :uri "/hunterkit"
            :port 9701
            :document-base (merge-pathnames "assets/" (asdf:system-source-directory 'hunter-kit))
            :template (merge-pathnames "assets/main.tmpl" (asdf:system-source-directory 'hunter-kit))
            :css ("libs/bootstrap/css/bootstrap.css"
                  "libs/bootstrap/css/bootstrap-responsive.css")
            :libs (;; JQuery from Google Ajax cdn
		   "http://ajax.googleapis.com/ajax/libs/jquery/1.10.1/jquery.min.js"
		   ;; underscore.js from cdnjs
		   "http://cdnjs.cloudflare.com/ajax/libs/underscore.js/1.4.4/underscore-min.js"
		   ;; backbone.js from cdnjs
                   "http://cdnjs.cloudflare.com/ajax/libs/backbone.js/1.0.0/backbone-min.js"
                   ;; bootstrap
                   "libs/bootstrap/js/bootstrap.min.js"))
  '(defvar app (new (web-app-router)))
  '((@ *backbone history start)))


