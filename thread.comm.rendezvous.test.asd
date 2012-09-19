#|
  This file is a part of thread.comm.rendezvous project.
  Copyright (c) 2011 Kazuo Koga
|#

(in-package :cl-user)
(defpackage cl-rendezvous-test-asd
  (:use :cl :asdf))
(in-package :cl-rendezvous-test-asd)

(defsystem :thread.comm.rendezvous.test
  :version "12.9.19"
  :author "Kazuo Koga"
  :license "MIT"
  :depends-on (:thread.comm.rendezvous
               :cl-test-more)
  :components ((:module "t"
                :components
                ((:file "rendezvous")))))
