(import (scheme base)
	(rapid test)
	(rename (rapid macros test) (run-tests run-rapid-macros-tests)))

(test-begin "Rapid Macros")

(run-rapid-macros-tests)

(test-end "Rapid Macros")
