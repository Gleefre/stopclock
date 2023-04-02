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

(defpackage #:stopclock/tests
  (:use #:cl #:stopclock #:fiveam)
  (:shadowing-import-from #:stopclock
                          #:time #:run #:speed))

(in-package #:stopclock/tests)

(def-suite :stopclock
  :description "Main suite")

(def-suite* clock-initialization :in :stopclock
  :description "Test the initialization of a clock")

(test make-clock-clock-p
  (let ((c (make-clock)))
    (is (clock-p c))))

(test make-clock-state
  (let ((c (make-clock :paused t)))
    (is-true (paused c)))
  (let ((c (make-clock :paused nil)))
    (is-false (paused c)))
  (let ((c (make-clock)))
    (is-false (paused c))))

(test make-clock-time-speed
  (for-all ((speed (gen-integer :min 1)))
    (let ((c+ (make-clock :speed speed))
          (c- (make-clock :speed (- speed))))
      (is-false (minusp (time c+)))
      (is-false (plusp (time c-)))
      (sleep 1/100)
      (is-true (plusp (time c+)))
      (is-true (minusp (time c-))))))

(test make-clock-zero-speed
  (signals zero-clock-speed-error (make-clock :speed 0)))

(test make-clock-time
  (for-all ((time (gen-float)))
    (is (= time (time (make-clock :time time :paused t))))))

(def-suite* clock-state :in :stopclock
  :description "Test changing the state of the clock")

(test clock-stop-and-run
  (let ((c (make-clock)))
    (is-true (paused (stop c)))
    (is-true (paused (stop c)))
    (is-false (paused (run c)))
    (is-false (paused (run c)))
    (is-true (paused (pause c)))
    (is-true (paused (pause c)))
    (is-false (paused (start c)))
    (is-false (paused (start c)))))

(test clock-toggle
  (let ((c (stop (make-clock))))
    (is-false (paused (toggle c)))
    (is-true (paused (toggle c)))
    (is-false (paused (toggle c)))
    (is-true (paused (toggle c)))))

(test clock-setf-paused
  (let ((c (make-clock)))
    (setf (paused c) t)
    (is-true (paused c))
    (setf (paused c) t)
    (is-true (paused c))
    (setf (paused c) nil)
    (is-false (paused c))
    (setf (paused c) nil)
    (is-false (paused c))
    (setf (paused c) t)
    (is-true (paused c))))

(test clock-state-return-value
  (let ((c (make-clock)))
    (is (eq c (stop c)))
    (is (eq c (start c)))
    (is (eq c (pause c)))
    (is (eq c (run c)))
    (is (eq c (toggle c)))
    (is (eq c (toggle c)))
    (is (eq t (setf (paused c) t)))
    (is (eq t (setf (paused c) t)))
    (is (eq nil (setf (paused c) nil)))
    (is (eq nil (setf (paused c) nil)))))

(def-suite* clock-time :in :stopclock
  :description "Test changing the time of the clock")

(test clock-adjust
  (let ((c (make-clock :paused t))
        (sum 0))
    (is-true (zerop (time c)))
    (for-all ((adjust (gen-integer)))
      (incf sum adjust)
      (adjust c adjust)
      (is (= sum (time c))))))

(test clock-setf-time
  (let ((c (make-clock :paused t)))
    (for-all ((time (gen-integer)))
      (setf (time c) time)
      (is (= time (time c))))
    (for-all ((time (gen-float)))
      (setf (time c) time)
      (is (= time (time c))))))

(test clock-time-return-value
  (let ((c (make-clock)))
    (is (eq c (adjust c 0)))
    (stop c)
    (is (= 13 (setf (time c) 13)))))

(def-suite* clock-time-speed :in :stopclock
  :description "Test changing the speed of the clock")

(test clock-accelerate
  (let ((c (make-clock)))
    (signals zero-clock-speed-error
      (accelerate c 0))
    (setf (time (stop c)) 2)
    (for-all ((factor (gen-integer :min 1))
              (sign (gen-one-element -1 1)))
      (let ((speed (speed c)))
        (accelerate c (* sign factor))
        (is (= (speed c) (* speed factor sign)))
        (is (= 2 (time c)))))))

(test clock-setf-speed
  (let ((c (make-clock)))
    (signals zero-clock-speed-error
      (setf (speed c) 0))
    (setf (time (stop c)) 2)
    (for-all ((speed (gen-integer :min 1))
              (sign (gen-one-element -1 1)))
      (setf (speed c) (* speed sign))
      (is (= (speed c) (* speed sign)))
      (is (= 2 (time c))))))

(test clock-speed-return-value
  (let ((c (make-clock)))
    (is (eq c (accelerate c -1)))
    (is (= 10 (setf (speed c) 10)))))

(def-suite* clock-reset :in :stopclock
  :description "Test the reset function")

(test clock-reset-time
  (let ((c (make-clock :time 100)))
    (reset c :paused t)
    (is (= 0 (time c)))
    (for-all ((time (gen-float)))
      (reset c :paused t :time time)
      (is (= time (time c))))
    (for-all ((time (gen-integer))
              (speed-absolute-value (gen-integer :min 1))
              (sign (gen-one-element 1 -1)))
      (let ((speed (* sign speed-absolute-value)))
        (reset c :paused t :time time :speed speed)
        (is (= time (time c)))))))

(test clock-reset-state
  (let ((c (make-clock)))
    (reset c)
    (is-false (paused c))
    (reset c :paused t)
    (is-true (paused c))
    (reset c)
    (is-true (paused c))
    (reset c :run t)
    (is-false (paused c))
    (reset c)
    (is-false (paused c))))

(test clock-reset-paused-over-run
  (let ((c (make-clock)))
    (reset c :paused t :run t)
    (is-true (paused c))
    (reset c :paused t :run t)
    (is-true (paused c))))

(test clock-reset-return-value
  (let ((c (make-clock)))
    (is (eq c (reset c)))
    (is (eq c (reset c :run t)))
    (is (eq c (reset c :paused t)))
    (is (eq c (reset c :run t :paused t)))
    (is (eq c (reset c :speed 4)))
    (is (eq c (reset c :run t :speed 4)))
    (is (eq c (reset c :paused t :speed 4)))))

(def-suite* clock-advanced :in :stopclock
  :description "Test the time source, clock freeze, copy-clock")

(test synchronized-clocks
  (let* ((clock (make-clock :paused t))
         (1x (make-clock :time-source (lambda () (time clock))))
         (latency (sleep 0.01))
         (5x (make-clock :time-source (lambda () (time clock))
                               :speed 5)))
    (declare (ignore latency))
    (run clock)
    (sleep 1)
    (stop clock)
    (is (= (* 5 (time 1x))
           (time 5x)))))

(test clocks-over-common-source
  (let* ((c  (make-clock :paused t))
         (c* (make-clock :paused t :time-source 'run-time))
         (a  (make-clock :time-source c))
         (a* (make-clock :time-source c*))
         (b  (make-clock :time-source c :speed 5))
         (b* (make-clock :time-source c* :speed 5)))
    (loop repeat 100
          do (sleep 0.01)
          do (is-true (with-freeze c
                        (= (time c) (time a) (/ (time b) 5))))
          do (is-true (with-freeze c*
                        (= (time c*) (time a*) (/ (time b*) 5)))))
    (run c)
    (run c*)
    (loop repeat 100
          do (sleep 0.01)
          do (is-true (with-freeze c
                        (= (time c) (time a) (/ (time b) 5))))
          do (is-true (with-freeze c*
                        (= (time c*) (time a*) (/ (time b*) 5)))))))

(test clock-copies-over-freezed-source
  (let* ((c (make-clock :paused t))
         (d (make-clock :time-source c))
         (e (copy-clock d)))
    (run c)
    (loop repeat 100
          do (sleep 0.01)
          do (is-true (with-freeze c
                        (= (time d) (time e)))))))
