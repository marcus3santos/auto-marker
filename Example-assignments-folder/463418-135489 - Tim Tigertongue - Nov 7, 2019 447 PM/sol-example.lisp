;; Example of a lab assignment solution

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
