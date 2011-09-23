#|
  This file is a part of thread.comm.rendezvous project.
  Copyright (c) 2011 Kazuo Koga
|#

#|
  Author: Kazuo Koga
|#

(in-package :cl-user)
(defpackage cl-rendezvous-asd
  (:use :cl :asdf))
(in-package :cl-rendezvous-asd)

(defsystem :thread.comm.rendezvous
  :version "11.9"
  :author "Kazuo Koga"
  :license "MIT"
  :depends-on (:cl-annot :bordeaux-threads)
  :components ((:module "src"
                :components
                ((:file "rendezvous"))))
  :description "Rendezvous thread synchronization"
  :long-description
  #.(with-open-file (stream (merge-pathnames
                             #p"README.markdown"
                             (or *load-pathname* *compile-file-pathname*))
                            :direction :input)
      (let ((seq (make-array (file-length stream)
                             :element-type 'character
                             :fill-pointer t)))
        (setf (fill-pointer seq) (read-sequence seq stream))
        seq)))
