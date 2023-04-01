(defsystem "clock"
  :description "clock is a library for measuring time using clocks"
  :version "0.0.6"
  :author "Grolter <varedif.a.s@gmail.com>"
  :license "Apache 2.0"
  :components ((:file "clock")))

(defsystem "clock/tests"
  :description "test system for clock"
  :author "Grolter <varedif.a.s@gmail.com>"
  :license "Apache 2.0"
  :depends-on ("clock" "fiveam")
  :components ((:file "tests"))
  :perform (test-op (op c) (symbol-call :fiveam :run! :clock)))
