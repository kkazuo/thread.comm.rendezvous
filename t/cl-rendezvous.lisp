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
                :make-thread
                :thread-yield
                :all-threads))
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
  (make-thread (lambda () (accept-rendezvous rdv)))
  (is (call-rendezvous rdv 2) 2))
(let ((rdv (make-rendezvous))
      (count (length (all-threads))))
  (loop repeat 100
        do (make-thread (lambda () (accept-rendezvous rdv))))
  (loop repeat 100
        do (make-thread (lambda () (call-rendezvous rdv 0))))
  (loop until (= count (length (all-threads)))
        do (thread-yield))
  (pass "many accept many call"))
(let ((rdv (make-rendezvous))
      (count (length (all-threads))))
  (loop repeat 100
        do (make-thread (lambda () (call-rendezvous rdv 0))))
  (loop repeat 100
        do (make-thread (lambda () (accept-rendezvous rdv))))
  (loop until (= count (length (all-threads)))
        do (thread-yield))
  (pass "many call many accept"))
(let ((rdv (make-rendezvous))
      (count (length (all-threads))))
  (loop repeat 100
        do (make-thread (lambda () (call-rendezvous rdv 0)))
           (make-thread (lambda () (accept-rendezvous rdv))))
  (loop repeat 100
        do (make-thread (lambda () (accept-rendezvous rdv)))
           (make-thread (lambda () (call-rendezvous rdv 0))))
  (loop until (= count (length (all-threads)))
        do (thread-yield))
  (pass "many call accept"))

(diag "Extended rendezvous.")
(let ((rdv (make-rendezvous)))
  (make-thread (lambda () (accept-rendezvous rdv
                                             :reply (lambda (value) (* value 7)))))
  (is (call-rendezvous rdv 3) (* 3 7)))

(finalize)
