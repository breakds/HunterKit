;;;; package.lisp
;;;; Package definition for hunter-kit

(defpackage #:breakds.hunter-kit
  (:nicknames #:hunter-kit)
  (:use #:cl
        #:lazy-bone
        #:hunchentoot
        #:exmac)
  (:import-from #:parenscript #:ps* #:ps #:create
                #:chain #:defpsmacro #:new #:getprop #:@ #:for-in #:eql)
  (:export *skills*
           *skills-array*
	   *jewels*
           *jewels-array*
	   *helms*
           *helms-array*
	   *chests*
           *chests-array*
	   *gloves*
           *gloves-array*
	   *belts*
           *belts-array*
	   *boots*
           *boots-array*
	   *armor-set*
	   init
	   get-id-by-name
	   search-armor
	   print-set
           ;;; the app
           start-server
           stop-server
           ;;; debug
           stuffed-armor
           stuffed-armor-def-max
           armor-def-max
           decombo
           *toot-output*
           *jewels*
           get-jewels
           make-skill-group
           make-skill-group-row
           make-skill-group-all
           get-armor-list
           carriable-name
           group))



           





