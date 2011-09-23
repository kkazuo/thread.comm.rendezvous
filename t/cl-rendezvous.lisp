#|
  This file is a part of cl-rendezvous project.
  Copyright (c) 2011 Kazuo Koga
|#

(in-package :cl-user)
(defpackage cl-rendezvous-test
  (:use :cl
        :cl-rendezvous
        :cl-test-more)
  (:import-from :bordeaux-threads
                :make-thread))
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

(diag "Thread contention.")
(let ((rdv (make-rendezvous)))
  (make-thread (lambda () (call-rendezvous rdv 1)))
  (is (accept-rendezvous rdv) 1))
(let ((rdv (make-rendezvous)))
  (make-thread (lambda () (is (accept-rendezvous rdv) 2)))
  (call-rendezvous rdv 2))

(diag "Extended rendezvous.")
(let ((rdv (make-rendezvous)))
  (make-thread (lambda () (accept-rendezvous rdv
                                             :reply (lambda (value) (* value 7)))))
  (is (call-rendezvous rdv 3) (* 3 7)))

(finalize)
