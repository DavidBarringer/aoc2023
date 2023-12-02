;; Put the given solutions for the examples here
(setf test-sol-a 8)
(setf test-sol-b 2286)

;; Turn the input file into whatever form you will use for both parts
;; (get-file-lines) and (get-file-string) will be useful
(defun parse-input (input-file)
  (let ((games (mapcar (lambda (s) (split ": " s)) (get-file-lines input-file))))
    (mapcar (lambda (g) (cons (parse-integer (CAR g) :start 5)
			      (mapcar (lambda (subsets)
					(mapcar (lambda (cubes) (cons (parse-integer (CAR (split " " cubes)))
								      (read-from-string (CADR (split " " cubes)))))
						(split ", " subsets)))
				      (split "; " (CADR g)))))
	    games)))

;; Returns the solution for part a
(defun part-a (parsed-input)
  (let ((red 12) (green 13) (blue 14))
    (declare (special red green blue))
    (loop for game in parsed-input
	  if (NOT (loop for configs in (CDR game)
			if (loop for (num . cube) in configs
				 if (> num (eval cube)) collect cube)
			  collect configs))
	  sum (CAR game))))

;; Returns the solution for part b
(defun part-b (parsed-input)
  (loop for game in parsed-input
	do (setf red 0 green 0 blue 0)
	sum (loop for configs in (CDR game)
		  do (loop for (num . cube) in configs
			   if (> num (eval cube)) do (setf (symbol-value cube) num))
		  finally (return (* red green blue)))))
