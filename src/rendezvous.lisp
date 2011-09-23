#|
  This file is a part of thread.comm.rendezvous project.
  Copyright (c) 2011 Kazuo Koga
|#

(in-package :cl-user)
(defpackage :thread.comm.rendezvous
  (:use :cl)
  (:import-from :bordeaux-threads
                :make-lock
                :with-lock-held
                :make-condition-variable
                :condition-wait
                :condition-notify)
  (:import-from :ccl
                :make-semaphore
                :signal-semaphore
                :wait-on-semaphore))
(in-package :thread.comm.rendezvous)
(annot:enable-annot-syntax)

@export
(defun nickname-package (&optional (nickname :rdv))
  "Add NICKNAME (:RDV by default) to the :THREAD.COMM.RENDEZVOUS package."
  (rename-package :thread.comm.rendezvous
                  (package-name :thread.comm.rendezvous)
                  (adjoin nickname (package-nicknames :thread.comm.rendezvous)
                          :test #'string-equal)))

(defun make-tconc ()
  (let ((cell (list 'tconc)))
    (cons cell cell)))

(defun tconc-empty-p (tconc)
  (declare (type cons tconc))
  (eq (car tconc) (cdr tconc)))

(defun push-tconc (tconc item)
  (declare (type cons tconc))
  (let ((cell (list item)))
    (setf (cddr tconc) cell)
    (setf (cdr tconc) cell))
    tconc)

(defun pop-tconc (tconc)
  (declare (type cons tconc))
  (prog1 (cadar tconc)
         (cond ((eq (cdar tconc) (cdr tconc))
                (setf (cdar tconc) nil)
                (setf (cdr tconc) (car tconc)))
               (t
                (setf (cdar tconc) (cddar tconc))))))

(defun tconc-list (tconc)
  (declare (type cons tconc))
  (cdar tconc))


(defclass rendezvous ()
     ((name         :initarg :name)
      (queue        :initform (make-tconc))
      (lock         :initform (make-lock))
      (condvar      :initform (make-condition-variable))
      (reader-count :initform 0)))

(defmethod print-object ((rendezvous rendezvous) stream)
  (print-unreadable-object (rendezvous stream :type t :identity t)
    (format stream "~A" (slot-value rendezvous 'name))))

@export
(defun make-rendezvous (&optional (name "Anonymous"))
  (make-instance 'rendezvous :name name))

@export
(defmethod rendezvous-name ((rendezvous rendezvous))
  (slot-value rendezvous 'name))

@export
(defmethod call-rendezvous ((rendezvous rendezvous) value)
  (with-slots (lock queue condvar reader-count) rendezvous
     (let* ((reader-signal (make-semaphore))
            (packet (cons reader-signal value)))
       (unwind-protect
           (with-lock-held (lock)
             (when (< 0 reader-count)
               (condition-notify condvar)
               (decf reader-count))
             (push-tconc queue packet))
         (wait-on-semaphore reader-signal))
       (cdr packet))))

@export
(defmethod accept-rendezvous ((rendezvous rendezvous) &key reply)
  (with-slots (lock queue condvar reader-count) rendezvous
     (with-lock-held (lock)
       (loop while (tconc-empty-p queue)
             do (incf reader-count)
                (condition-wait condvar lock))
       (let (writer)
         (unwind-protect
             (progn
               (setf writer (pop-tconc queue))
               (let ((value (cdr writer)))
                 (when reply
                   (setf (cdr writer) (funcall reply value)))
                 value))
           (when writer
             (signal-semaphore (car writer))))))))
