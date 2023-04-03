(defsystem "stopclock"
  :description "stopclock is a library for measuring time using (stop)clocks"
  :version "1.0.1"
  :author "Grolter <varedif.a.s@gmail.com>"
  :license "Apache 2.0"
  :components ((:file "stopclock"))
  :in-order-to ((test-op (test-op "stopclock/tests"))))

(defsystem "stopclock/tests"
  :description "test system for stopclock"
  :author "Grolter <varedif.a.s@gmail.com>"
  :license "Apache 2.0"
  :depends-on ("stopclock" "fiveam")
  :components ((:file "tests"))
  :perform (test-op (op c) (symbol-call :fiveam '#:run! :stopclock)))
