;;;; Copyright 2023 Gleefre
;;;;
;;;; Licensed under the Apache License, Version 2.0 (the "License");
;;;; you may not use this file except in compliance with the License.
;;;; You may obtain a copy of the License at
;;;;
;;;;     http://www.apache.org/licenses/LICENSE-2.0
;;;;
;;;; Unless required by applicable law or agreed to in writing, software
;;;; distributed under the License is distributed on an "AS IS" BASIS,
;;;; WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
;;;; See the License for the specific language governing permissions and
;;;; limitations under the License.

(defpackage #:stopclock
  (:use #:cl)
  (:shadow #:time #:speed)
  (:export #:clock #:make-clock #:clock-p #:copy-clock
           #:real-time #:run-time
           #:paused #:time #:speed
           #:adjust #:accelerate
           #:pause #:stop #:start #:run #:toggle
           #:freeze #:unfreeze #:with-freeze
           #:reset
           #:zero-clock-speed-error))

(in-package #:stopclock)

;;; Possible functions for time

(defun real-time ()
  (/ (get-internal-real-time)
     internal-time-units-per-second))

(defun run-time ()
  (/ (get-internal-run-time)
     internal-time-units-per-second))

;;; Zero clock speed condition

(define-condition zero-clock-speed-error (error)
  ((clock :initarg :clock
          :initform nil
          :reader clock)
   (message :initarg :message
            :initform "Clock speed cannot be equal to zero."))
  (:documentation "Condition of setting the clock speed equal to zero.")
  (:report (lambda (condition stream)
             (format stream
                     (slot-value condition 'message)
                     (clock condition)))))

;;; Clock structure

(defstruct (clock (:constructor make-clock%))
  start-time
  pause-time
  speed
  time-source
  freeze)

(declaim (ftype (function (clock) number) time))

(defun make-clock (&key (paused nil)
                        (speed 1)
                        ((:time-source time-source%) 'real-time)
                        (time 0)
                   &aux (time-source (if (clock-p time-source%)
                                         (lambda () (time time-source%))
                                         time-source%))
                        (current-time (funcall time-source)))
  (when (zerop speed)
    (error 'zero-clock-speed-error
           :message "You cannot create a clock with speed equal to zero."))
  (make-clock%
   :start-time (- current-time (/ time speed))
   :pause-time (when paused current-time)
   :speed speed
   :time-source time-source
   :freeze nil))

;;; A pair of internal macros

(defmacro with-a-clock-slots (clock &body body)
  `(with-slots (start-time pause-time time-source speed freeze)
       ,clock
     ,@body))

(defmacro a-now ()
  '(or pause-time (funcall time-source)))

;;; Clock time

(defun time (clock)
  "Returns the current time on the `clock'."
  (with-a-clock-slots clock
    (* (- (a-now) start-time)
       speed)))

(defun (setf time) (new-time clock)
  "Sets the current time on the `clock' to the `new-time'."
  (with-a-clock-slots clock
    ;; speed * (now - start) = time --> new-start = now - new-time / speed
    (setf start-time (- (a-now)
                        (/ new-time speed))))
  new-time)

(defun adjust (clock seconds)
  "Adds `seconds' seconds to the current time on the `clock', returns the `clock' itself."
  (with-a-clock-slots clock
    (decf start-time (/ seconds speed)))
  clock)

(defun reset (clock &key (time 0) ((:speed new-speed)) paused run)
  "Resets the `clock's state. By default, only the current time is reset to 0.
You can specify a new state for the `:speed', `:time' and whether the clock
should be `:paused' or `:run' (`:paused' takes precedence over `:run')."
  (with-a-clock-slots clock
    (let ((current-time (funcall time-source)))
      (setf start-time (- current-time (/ time (or new-speed speed)))
            pause-time (when (or paused
                                 (and pause-time (not run)))
                         current-time)
            speed (or new-speed speed))))
  clock)

;;; Clock speed

(defun speed (clock)
  "Returns the current `speed' of the `clock'."
  (clock-speed clock))

(defun accelerate (clock factor)
  "Accelerates the `speed' of the `clock' by `factor' times, returns the `clock' itself.
`factor' cannot be zero."
  (when (zerop factor)
    (error 'zero-clock-speed-error
           :clock clock
           :message "You cannot accelerate the speed of the clock by 0.~%  CLOCK: ~a"))
  (with-a-clock-slots clock
    ;; time = (now - start) * old-speed
    ;;      = (now - new-start) * old-speed * factor
    ;; now - start = (now - new-start) * factor
    ;; new-start = now - (now - start) / factor = now * (factor - 1) / factor + start / factor
    (setf start-time (+ (/ start-time factor)
                        (* (a-now)
                           (/ (1- factor) factor))))
    (setf speed (* speed factor)))
  clock)

(defun (setf speed) (new-speed clock)
  "Sets the `speed' of the `clock' to the `new-speed'.
`new-speed' cannot be zero."
  (when (zerop new-speed)
    (error 'zero-clock-speed-error
           :clock clock
           :message "You cannot set speed to be equal to zero.~%  CLOCK: ~a"))
  (accelerate clock (/ new-speed (speed clock)))
  (speed clock))

;;; Clock state

(defun stop (clock)
  "Stops the `clock', returns the `clock' itself. Synonymous to `pause' function."
  (with-a-clock-slots clock
    (setf pause-time (a-now)))
  clock)

(defun pause (clock)
  "Pauses the `clock', returns the `clock' itself. Synonymous to `stop' function."
  (with-a-clock-slots clock
    (setf pause-time (a-now)))
  clock)

(defun run (clock)
  "Runs the `clock', returns the `clock' itself. Synonymous to `start' function."
  (with-a-clock-slots clock
    (when pause-time
      (incf start-time (- (funcall time-source)
                          pause-time))
      (setf pause-time nil)))
  clock)

(defun start (clock)
  "Starts the `clock', returns the `clock' itself. Synonymous to `run' function."
  (with-a-clock-slots clock
    (when pause-time
      (incf start-time (- (funcall time-source)
                          pause-time))
      (setf pause-time nil)))
  clock)

(defun paused (clock)
  "Returns T if the `clock' is paused and NIL if it is running."
  (not (not (clock-pause-time clock))))

(defun (setf paused) (state clock)
  "Pauses the `clock' if `state' is T and runs it if `state' is NIL."
  (if state
      (stop clock)
      (run clock))
  (paused clock))

(defun toggle (clock)
  "Runs the `clock' if it was paused and stops it otherwise,
returns the `clock' itself."
  (if (paused clock)
      (run clock)
      (stop clock)))

;;; Freezing the clock

(defun freeze (clock)
  "Freezes the `clock': the time passed will be counted during `unfreeze' if clock is running."
  (with-a-clock-slots clock
    (unless pause-time
      (setf pause-time (funcall time-source)
            freeze t)))
  clock)

(defun unfreeze (clock)
  "Unfreezes the `clock': the time during the freeze is added if clock was running before the freeze."
  (with-a-clock-slots clock
    (when freeze
      (setf pause-time nil
            freeze nil)))
  clock)

(defmacro with-freeze (clock &body body &aux (clock-name (gensym "clock")))
  "Locally freezes the clock."
  `(let ((,clock-name ,clock))
     (unwind-protect
          (progn
            (freeze ,clock-name)
            ,@body)
       (unfreeze ,clock-name))))

;;; Print method

(defmethod print-object ((clock clock) stream)
  (print-unreadable-object (clock stream)
    (format stream "~:@(clock :time ~,2f seconds ~s :speed~) ~:[-~;~]x~a"
            (time clock)
            (cond ((clock-freeze clock) :freezed)
                  ((paused clock) :paused)
                  (t :running))
            (plusp (speed clock))
            (abs (speed clock)))))
