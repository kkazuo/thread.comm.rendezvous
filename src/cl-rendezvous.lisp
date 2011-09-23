#|
  This file is a part of cl-rendezvous project.
  Copyright (c) 2011 Kazuo Koga
|#

(in-package :cl-user)
(defpackage cl-rendezvous
  (:nicknames :rendezvous)
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
(in-package :cl-rendezvous)

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
     ((queue   :initform (make-tconc))
      (lock    :initform (make-lock))
      (condvar :initform (make-condition-variable))))

(defmethod call-rendezvous ((rendezvous rendezvous) value)
  (with-slots (lock queue condvar) rendezvous
     (let* ((reader-signal (make-semaphore))
            (packet (cons reader-signal value)))
       (unwind-protect
           (with-lock-held (lock)
             (when (tconc-empty-p queue)
               (condition-notify condvar))
             (push-tconc queue packet))
         (wait-on-semaphore reader-signal))
       (cdr packet))))

(defmethod accept-rendezvous ((rendezvous rendezvous) &key extended)
  (with-slots (lock queue condvar) rendezvous
     (with-lock-held (lock)
       (loop while (tconc-empty-p queue)
             do (condition-wait condvar lock))
       (let (writer)
         (unwind-protect
             (progn
               (setf writer (pop-tconc queue))
               (let ((value (cdr writer)))
                 (when extended
                   (setf (cdr writer) (funcall extended value)))
                 value))
           (when writer
             (signal-semaphore (car writer))))))))
