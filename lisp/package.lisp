;;;; package.lisp
;;;; Package definition for hunter-kit

(defpackage #:breakds.hunter-kit
  (:nicknames #:hunter-kit)
  (:use #:cl
        #:lazy-bone
        #:hunchentoot)
  (:import-from #:parenscript #:ps* #:ps #:create
                #:chain #:defpsmacro #:new #:getprop #:@ #:for-in #:eql)
  (:export *skills*
	   *jewels*
	   *helms*
	   *chests*
	   *gloves*
	   *belts*
	   *boots*
	   *armor-set*
	   init
	   get-id-by-name
	   search-armor
	   print-set
           ;;; the app
           start-server
           stop-server
           ;;; debug
           decombo
           *toot-output*
           make-skill-group
           make-skill-group-row
           make-skill-group-all
           group))



           





