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

(defpackage #:clock/tests
  (:use #:cl #:clock #:fiveam)
  (:shadowing-import-from #:clock
                          #:time #:run))

(in-package #:clock/tests)

(def-suite :clock
  :description "Main suite")

(def-suite* clock-initialization :in :clock
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

(test make-clock-time-flow
  (for-all ((flow (gen-integer :min 1)))
    (let ((c+ (make-clock :time-flow flow))
          (c- (make-clock :time-flow (- flow))))
      (is-false (minusp (time c+)))
      (is-false (plusp (time c-)))
      (sleep 1/100)
      (is-true (plusp (time c+)))
      (is-true (minusp (time c-))))))

(test make-clock-zero-flow
  (signals zero-time-flow-error (make-clock :time-flow 0)))

(test make-clock-time
  (for-all ((time (gen-float)))
    (is (= time (time (make-clock :time time :paused t))))))

(def-suite* clock-state :in :clock
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

(def-suite* clock-time :in :clock
  :description "Test changing the time of the clock")

(test clock-shift
  (let ((c (make-clock :paused t))
        (sum 0))
    (is-true (zerop (time c)))
    (for-all ((shift (gen-integer)))
      (incf sum shift)
      (shift c shift)
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
    (is (eq c (shift c 0)))
    (stop c)
    (is (= 13 (setf (time c) 13)))))

(def-suite* clock-time-flow :in :clock
  :description "Test changing the time flow of the clock")

(test clock-accelerate
  (let ((c (make-clock)))
    (signals zero-time-flow-error
      (accelerate c 0))
    (setf (time (stop c)) 2)
    (for-all ((flow (gen-integer :min 1))
              (sign (gen-one-element -1 1)))
      (let ((old-flow (time-flow c)))
        (accelerate c (* sign flow))
        (is (= (time-flow c) (* old-flow flow sign)))
        (is (= 2 (time c)))))))

(test clock-setf-time-flow
  (let ((c (make-clock)))
    (signals zero-time-flow-error
      (setf (time-flow c) 0))
    (setf (time (stop c)) 2)
    (for-all ((flow (gen-integer :min 1))
              (sign (gen-one-element -1 1)))
      (setf (time-flow c) (* flow sign))
      (is (= (time-flow c) (* flow sign)))
      (is (= 2 (time c))))))

(test clock-time-flow-return-value
  (let ((c (make-clock)))
    (is (eq c (accelerate c -1)))
    (is (= 10 (setf (time-flow c) 10)))))

(def-suite* clock-reset :in :clock
  :description "Test the reset function")

(test clock-reset-time
  (let ((c (make-clock :time 100)))
    (reset c :paused t)
    (is (= 0 (time c)))))

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
    (is (eq c (reset c :time-flow 4)))
    (is (eq c (reset c :run t :time-flow 4)))
    (is (eq c (reset c :paused t :time-flow 4)))))

(def-suite* clock-advanced :in :clock
  :description "Test the time source, clock freeze, copy-clock")

(test synchronized-clocks
  (let* ((clock (clock:make-clock :paused t))
         (1x (clock:make-clock :time-source (lambda () (clock:time clock))))
         (latency (sleep 0.01))
         (5x (clock:make-clock :time-source (lambda () (clock:time clock))
                               :time-flow 5)))
    (declare (ignore latency))
    (clock:run clock)
    (sleep 1)
    (clock:stop clock)
    (is (= (* 5 (clock:time 1x))
           (clock:time 5x)))))

(test clocks-over-common-source
  (let* ((c  (make-clock :paused t))
         (c* (make-clock :paused t :time-source 'run-time))
         (a  (make-clock :time-source c))
         (a* (make-clock :time-source c*))
         (b  (make-clock :time-source c :time-flow 5))
         (b* (make-clock :time-source c* :time-flow 5)))
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
