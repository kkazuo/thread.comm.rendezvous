#|
  This file is a part of thread.comm.rendezvous project.
  Copyright (c) 2011 Kazuo Koga
|#

(in-package :cl-user)
(defpackage cl-rendezvous-test
  (:use :cl
        :thread.comm.rendezvous
        :cl-test-more)
  (:import-from :bordeaux-threads
                :make-lock
                :with-lock-held
                :make-thread
                :thread-yield))
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
      (lock (make-lock))
      (done 0))
  (loop repeat 100
        do (make-thread (lambda () (accept-rendezvous rdv))))
  (loop repeat 100
        do (make-thread (lambda () (call-rendezvous rdv 0)
                          (with-lock-held (lock) (incf done)))))
  (loop until (= 100 done)
        do (thread-yield))
  (pass "many accept many call"))
(let ((rdv (make-rendezvous))
      (lock (make-lock))
      (done 0))
  (loop repeat 100
        do (make-thread (lambda () (call-rendezvous rdv 0)
                          (with-lock-held (lock) (incf done)))))
  (loop repeat 100
        do (make-thread (lambda () (accept-rendezvous rdv))))
  (loop until (= 100 done)
        do (thread-yield))
  (pass "many call many accept"))
(let ((rdv (make-rendezvous))
      (lock (make-lock))
      (done 0))
  (loop repeat 100
        do (make-thread (lambda () (call-rendezvous rdv 0)
                          (with-lock-held (lock) (incf done))))
           (make-thread (lambda () (accept-rendezvous rdv))))
  (loop repeat 100
        do (make-thread (lambda () (accept-rendezvous rdv)))
           (make-thread (lambda () (call-rendezvous rdv 0)
                          (with-lock-held (lock) (incf done)))))
  (loop until (= 200 done)
        do (thread-yield))
  (pass "many call accept"))

(diag "Extended rendezvous.")
(let ((rdv (make-rendezvous)))
  (make-thread (lambda () (accept-rendezvous rdv
                                             :reply (lambda (value) (* value 7)))))
  (is (call-rendezvous rdv 3) (* 3 7)))

(finalize)
