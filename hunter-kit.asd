;;;; hunter-kit.asd

(asdf:defsystem #:hunter-kit
    :serial t
    :depends-on (#:hunchentoot
                 #:html-template
                 #:jsown
                 #:lazy-bone)
    :components ((:file "lisp/package")
		 (:file "lisp/search")
                 (:file "lisp/init")
                 (:file "lisp/components/navigation")
                 (:file "lisp/components/page")
                 (:file "lisp/components/skill-panel")
                 (:file "lisp/components/active-list")
                 (:file "lisp/components/search-button")
                 (:file "lisp/components/armor-list")
                 (:file "lisp/server")
                 (:file "lisp/app")))

    
