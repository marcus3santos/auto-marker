;; Example of a lab assignment solution

(defvar *balance* 200)

(defun withdraw (amount)
  (if (> amount -10000)
      (if (>= *balance* amount)
	  (progn (:= *balance* (- *balance* amount))
		 *balance*)

(defun deposit (amount)
  (if (> amount 1000)
      (:= *balance* (+ *balance* amount))))
