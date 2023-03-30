(defpackage #:clock
  (:use :cl)
  (:shadow #:time)
  (:export #:clock
           #:make-clock
           #:paused #:stop #:run #:toggle
           #:time #:shift #:reset))

(in-package #:clock)

;;; Clock struct

(defstruct (clock (:constructor make-clock%))
  start-time
  pause-time
  paused)

(defun make-clock (&optional (paused t)
                   &aux (current-time (get-internal-real-time)))
  (make-clock%
   :start-time current-time
   :pause-time current-time
   :paused paused))

;; Declaiming functions to prevent warnings of undefined function
(declaim (ftype function paused stop run))

;;; Clock time

(defun time (clock)
  (/ (- (if (paused clock)
            (clock-pause-time clock)
            (get-internal-real-time))
        (clock-start-time clock))
     internal-time-units-per-second))

(defun (setf time) (new-time clock)
  (setf (clock-start-time clock)
        (- (if (paused clock)
               (clock-pause-time clock)
               (get-internal-real-time))
           (* new-time
              internal-time-units-per-second))))

(defun shift (clock seconds)
  (decf (clock-start-time clock)
        (* seconds internal-time-units-per-second)))

(defun reset (clock &optional (pause nil pause-p)
              &aux (current-time (get-internal-real-time)))
  (with-slots (start-time pause-time paused)
      clock
    (setf start-time current-time
          pause-time current-time)
    (when pause-p
      (setf paused pause))))

;;; Clock state

(defun paused (clock)
  (clock-paused clock))

(defun (setf paused) (state clock)
  (if state
      (stop clock)
      (run clock)))

(defun stop (clock)
  (unless (paused clock)
    (setf (clock-paused clock) t
          (clock-pause-time clock) (get-internal-real-time)))
  clock)

(defun run (clock)
  (when (paused clock)
    (incf (clock-start-time clock) (- (get-internal-real-time)
                                      (clock-pause-time clock)))
    (setf (clock-paused clock) nil))
  clock)

(defun toggle (clock)
  (if (paused clock)
      (run clock)
      (stop clock)))

;;; Pretty printing

(defun sec-to-str (sec &aux (ms (floor sec 1/1000)) spec)
  (format nil "~{~2,'0d:~2,'0d:~2,'0d,~3,'0d~}"
          (dolist (dt '(1000 60 60 24) spec)
            (push (mod ms dt) spec)
            (setf ms (floor ms dt)))))

(defmethod print-object ((clock clock) stream)
  (format stream "<~a elapsed, ~:@(~:[running~;paused~]~)>"
          (sec-to-str (time clock))
          (paused clock)))
