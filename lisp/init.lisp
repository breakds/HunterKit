(in-package #:breakds.hunter-kit)

(clear-tmpl)
(set-template-registry (merge-pathnames #P"assets/template/"
                                        (asdf:system-source-directory 'hunter-kit)))



