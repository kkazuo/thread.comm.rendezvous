#|
  This file is a part of cl-rendezvous project.
  Copyright (c) 2011 Kazuo Koga
|#

(in-package :cl-user)
(defpackage cl-rendezvous-test
  (:use :cl
        :cl-rendezvous
        :cl-test-more))
(in-package :cl-rendezvous-test)

(plan nil)

(diag "Instance creation.")
(ok (make-rendezvous))
(ok (make-rendezvous "object name"))
(ok (make-rendezvous 'object-name))
(is (rendezvous-name (make-rendezvous "name")) "name")
(is (rendezvous-name (make-rendezvous (string 'object-name))) "OBJECT-NAME")
(ok (loop repeat 10000
          collect (make-rendezvous)))

(finalize)
