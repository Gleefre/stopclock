(defpackage #:clock
  (:use :cl)
  (:shadow #:time)
  (:export #:clock #:make-clock
           #:real-time #:run-time
           #:paused #:time #:time-flow
           #:shift #:accelerate
           #:stop #:run #:toggle
           #:reset
           #:zero-time-flow-error))

(in-package #:clock)

;;; Possible functions for time

(defun real-time ()
  (/ (get-internal-real-time)
     internal-time-units-per-second))

(defun run-time ()
  (/ (get-internal-run-time)
     internal-time-units-per-second))

;;; Zero-flow condition

(define-condition zero-time-flow-error (error)
  ((clock :initarg :clock
          :initform nil
          :reader clock)
   (message :initarg :message
            :initform "Time flow cannot be equal to zero."))
  (:documentation "Condition of setting the time-flow equal to zero.")
  (:report (lambda (condition stream)
             (format stream
                     (slot-value condition 'message)
                     (clock condition)))))

;;; Clock structure

(defstruct (clock (:constructor make-clock%))
  start-time
  pause-time
  time-flow
  time-source)

(defun make-clock (&key (paused t)
                        (time-flow 1)
                        (time-source #'real-time)
                   &aux (current-time (funcall time-source)))
  (when (zerop time-flow)
    (error 'zero-time-flow-error
           :message "You cannot create a clock with time-flow equal to zero."))
  (make-clock%
   :start-time current-time
   :pause-time (when paused current-time)
   :time-flow time-flow
   :time-source time-source))

;;; A pair of internal macros

(defmacro with-a-clock-slots (clock &body body)
  `(with-slots (start-time pause-time time-source time-flow)
       ,clock
     ,@body))

(defmacro a-now ()
  '(or pause-time (funcall time-source)))

;;; Clock time

(defun time (clock)
  "Returns the current time on the `clock'."
  (with-a-clock-slots clock
    (* (- (a-now) start-time)
       time-flow)))

(defun (setf time) (new-time clock)
  "Sets the current time on the `clock' to the `new-time'."
  (with-a-clock-slots clock
    ;; flow * (now - start) = time --> new-start = now - new-time / flow
    (setf start-time (- (a-now)
                        (/ new-time time-flow)))))

(defun shift (clock seconds)
  "Adds `seconds' seconds to the current time on the `clock'."
  (with-a-clock-slots clock
    (decf start-time (/ seconds time-flow))))

(defun reset (clock &key paused run ((:time-flow flow)))
  "Resets the `clock's state. By default, only the current time is reset.
You can specify a new state for the `:time-flow' and whether the clock
should be `:paused' or `:run' (`:paused' takes precedence over `:run')."
  (with-a-clock-slots clock
    (let ((current-time (funcall time-source)))
      (setf start-time current-time
            pause-time (when (or paused
                                 (and pause-time (not run)))
                         current-time)
            time-flow (or flow time-flow)))))

;;; Time flow

(defun time-flow (clock)
  "Returns the current `time-flow' of the `clock'."
  (clock-time-flow clock))

(defun (setf time-flow) (new-flow clock)
  "Sets the `time-flow' of the `clock' to the `new-flow'.
`new-flow' cannot be zero."
  (when (zerop new-flow)
    (error 'zero-time-flow-error
           :clock clock
           :message "You cannot set time-flow to be equal to zero.~%  CLOCK: ~a"))
  (accelerate clock (/ new-flow
                       (time-flow clock))))

(defun accelerate (clock factor)
  "Accelerates the `time-flow' of the `clock' by `factor' times.
`factor' cannot be zero."
  (when (zerop factor)
    (error 'zero-time-flow-error
           :clock clock
           :message "You cannot accelerate the time-flow by 0.~%  CLOCK: ~a"))
  (with-a-clock-slots clock
    ;; time = (now - start) * old-flow
    ;;      = (now - new-start) * old-flow * factor
    ;; now - start = (now - new-start) * factor
    ;; new-start = now - (now - start) / factor = now * (factor - 1) / factor + start / factor
    (setf start-time (+ (/ start-time factor)
                        (* (a-now)
                           (/ (1- factor) factor))))
    (setf time-flow (* time-flow factor))))

;;; Clock state

(defun paused (clock)
  "Returns T if the `clock' is paused and NIL if it is running."
  (not (not (clock-pause-time clock))))

(defun (setf paused) (state clock)
  "Pauses the `clock' if `state' is T and runs it if `state' is NIL."
  (if state
      (stop clock)
      (run clock)))

(defun stop (clock)
  "Pauses the `clock', returns the `clock' itself."
  (with-a-clock-slots clock
    (setf pause-time (a-now)))
  clock)

(defun run (clock)
  "Runs the `clock', returns the `clock' itself."
  (with-a-clock-slots clock
    (when pause-time
      (incf start-time (- (funcall time-source)
                          pause-time))
      (setf pause-time nil)))
  clock)

(defun toggle (clock)
  "Runs the `clock' if it was paused and stops it otherwise,
returns the `clock' itself."
  (if (paused clock)
      (run clock)
      (stop clock)))

;;; Print method

(defmethod print-object ((clock clock) stream)
  (print-unreadable-object (clock stream)
    (format stream "~:@(:time ~,2f seconds :~:[running~;paused~] :time-flow~) ~:[-~;~]x~a"
            (time clock)
            (paused clock)
            (plusp (time-flow clock))
            (abs (time-flow clock)))))
