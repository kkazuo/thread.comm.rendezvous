#|
  This file is a part of cl-rendezvous project.
  Copyright (c) 2011 Kazuo Koga
|#

#|
  Author: Kazuo Koga
|#

(in-package :cl-user)
(defpackage cl-rendezvous-asd
  (:use :cl :asdf))
(in-package :cl-rendezvous-asd)

(defsystem cl-rendezvous
  :version "0.1-SNAPSHOT"
  :author "Kazuo Koga"
  :license "new BSD"
  :depends-on (:cl-annot :bordeaux-threads)
  :components ((:module "src"
                :components
                ((:file "cl-rendezvous"))))
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
