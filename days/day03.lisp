;; Put the given solutions for the examples here
(setf test-sol-a 4361)
(setf test-sol-b 467835)

;; Turn the input file into whatever form you will use for both parts
;; (get-file-lines) and (get-file-string) will be useful
(defun parse-input (input-file)
  (let ((l (mapcar (lambda (s) (concatenate 'list s)) (get-file-lines input-file))))
    (apply 'mapcar 'append
	   (loop for y in l for i from 0 collect (loop for x in y for j from 0 with num = nil with coords = nil
						       if (digit-char-p x)
							 do (setf num (cons x num))
							    (setf coords (cons (cons j i) coords))
							 and if (= j (- (length y) 1))
							       collect (cons (parse-integer (concatenate 'string (reverse num))) coords) into numbers
						       end
						       else
							 if num
							   collect (cons (parse-integer (concatenate 'string (reverse num))) coords) into numbers
							   and do (setf num nil coords nil)
						       end
						       if (symbol-p x)
							       collect (cons x (cons j i)) into symbols
						       finally
							  (return (list numbers symbols)))))))

(defun symbol-p (c)
  (AND (NOT (digit-char-p c)) (NOT (eq c #\.))))

(defun symbol-adj (num-coords symbols)
  (loop for (nil x . y) in symbols
	  thereis (loop for i from -1 to 1
			  thereis (loop for j from -1 to 1
					  thereis (find (cons (+ x j) (+ y i)) num-coords :test 'equal)))))

(defun gear-adj (symbol-coords nums)
  (let ((adj-nums (loop for num in nums
			if (loop for (x . y) in (CDR num)
				   thereis (loop for i from -1 to 1
						   thereis (loop for j from -1 to 1
								   thereis (equal symbol-coords (cons (+ x j) (+ y i))))))
			  collect (CAR num))))
    (if (= 2 (length adj-nums)) (apply '* adj-nums) 0)))

;; Returns the solution for part a
(defun part-a (parsed-input)
  (let ((nums (CAR parsed-input)) (symbols (CADR parsed-input)))
    (loop for num in nums if (symbol-adj (CDR num) symbols) sum (CAR num))))

;; Returns the solution for part b
(defun part-b (parsed-input)
  (let ((nums (CAR parsed-input)) (symbols (CADR parsed-input)))
    (loop for symbol in symbols if (eql #\* (CAR symbol)) sum (gear-adj (CDR symbol) nums))))
