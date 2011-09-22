#|
  This file is a part of cl-rendezvous project.
  Copyright (c) 2011 Kazuo Koga
|#

(in-package :cl-user)
(defpackage cl-rendezvous
  (:use :cl))
(in-package :cl-rendezvous)

(defun make-tconc ()
  (let ((cell (list 'tconc)))
    (cons cell cell)))

(defun tconc-empty-p (tconc)
  (eq (car tconc) (cdr tconc)))

(defun push-tconc (tconc item)
  (let ((cell (list item)))
    (setf (cddr tconc) cell)
    (setf (cdr tconc) cell))
    tconc)

(defun pop-tconc (tconc)
  (prog1 (cadar tconc)
         (if (eq (setf (cdar tconc) (cddar tconc))
                 (cdr tconc))
             (setf (cdr tconc) (car tconc)))))
