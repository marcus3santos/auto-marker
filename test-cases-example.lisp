;; Example of test cases we will use to test and mark an assignment submission

(deftest test-withdraw ()
  (:= *balance* 100)
  (check 
    (equal (withdraw 10) 90)
    (equal (withdraw 20) 70)
    (not (withdraw 10001))
    (not (withdraw 80))
    (equal (withdraw 60) 10)))

(deftest test-deposit ()
  (:= *balance* 100)
  (check
    (not (deposit 10001))
    (equal (deposit 10) 110)
    (equal (deposit 20) 130)))


(defun unit-test ()
  "Calls the test cases and 'forgets' the functions that were tested."
  (test-withdraw)
  (fmakunbound 'withdraw) ; Removes the function definition from the global environment,
			  ; so the next time around the unit test is done on a freshly loaded version of this function.
  (test-deposit)
  (fmakunbound 'deposit))

(unit-test) 

