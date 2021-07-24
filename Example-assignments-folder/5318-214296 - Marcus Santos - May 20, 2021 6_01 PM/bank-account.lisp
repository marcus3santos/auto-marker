;; Unit test macros

(defvar *test-name* nil)

;; (defun report-result (result form)
;;   (format t "~:[FAIL~;pass~] ...~a: ~a~%" result *test-name* form)
;;   result)

(defmacro with-gensyms ((&rest names) &body body)
  `(let ,(loop for n in names collect `(,n (gensym)))
     ,@body))

(defmacro combine-results (&body forms)
  (with-gensyms (result)
    `(let ((,result t))
       ,@(loop for f in forms collect `(unless ,f (setf ,result nil)))
       ,result)))
					   
(defmacro check (&body forms)
  `(combine-results
     ,@(loop for f in forms collect `(report-result ,f ',f))))

(defmacro deftest (name parameters &body body)
  `(defun ,name ,parameters
     (let ((*test-name* (append *test-name* (list ',name))))
       ,@body)))

;; Functions

(defvar *balance* 100)

(defun withdraw (amount)
  (if (> amount 10000)
      (format t "Max withdrawal is 10000.~%")
      (if (>= *balance* amount)
	  (progn (:= *balance* (- *balance* amount))
		 *balance*)
	  (format t "Insufficient funds~%"))))

(defun deposit (amount)
  (if (> amount 10000)
      (format t "Max deposit is 10000.~%")
      (:= *balance* (+ *balance* amount))))

;; Test cases

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


(defun main ()
  (test-withdraw)
  (test-deposit))
