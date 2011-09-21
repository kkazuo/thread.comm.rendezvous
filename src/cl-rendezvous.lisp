#|
  This file is a part of cl-rendezvous project.
  Copyright (c) 2011 Kazuo Koga
|#

(in-package :cl-user)
(defpackage cl-rendezvous
  (:use :cl))
(in-package :cl-rendezvous)

(defun make-tconc ()
  (list nil))

(defun tconc-empty-p (tconc)
  (null (cdr tconc)))

(defun push-tconc (tconc item)
  (let ((cell (list item)))
    (cond ((null (cdr tconc))
           (setf (car tconc) cell)
           (setf (cdr tconc) cell))
          (t
           (setf (cddr tconc) cell)
           (setf (cdr tconc) cell)))
    tconc))

(defun pop-tconc (tconc)
  (prog1 (caar tconc)
         (if (null (setf (car tconc) (cdar tconc)))
             (setf (cdr tconc) nil))))
