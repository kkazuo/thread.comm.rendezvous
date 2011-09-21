#|
  This file is a part of cl-rendezvous project.
  Copyright (c) 2011 Kazuo Koga
|#

(in-package :cl-user)
(defpackage cl-rendezvous-test-asd
  (:use :cl :asdf))
(in-package :cl-rendezvous-test-asd)

(defsystem cl-rendezvous-test
  :author "Kazuo Koga"
  :license "new BSD"
  :depends-on (:cl-rendezvous
               :cl-test-more)
  :components ((:module "t"
                :components
                ((:file "cl-rendezvous")))))
