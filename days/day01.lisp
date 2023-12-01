;; Put the given solutions for the examples here
(setf test-sol-a 142)
(setf test-sol-b 281)

;; Turn the input file into whatever form you will use for both parts
;; (get-file-lines) and (get-file-string) will be useful
(defun parse-input (input-file)
  (mapcar (lambda (s) (concatenate 'list s)) (get-file-lines input-file)))

;; Returns the solution for part a
(defun part-a (parsed-input)
  (loop for line in parsed-input sum
				 (loop for c in line if (digit-char-p c) collect c into digits
				       finally (return (parse-integer (concatenate 'string (cons (CAR digits) (last digits))))))))

;; Returns the solution for part b
(defun part-b (parsed-input)
  (part-a
   (loop for line in parsed-input collect
				  (concatenate 'list
					       (loop for i from 1 to 9 do (setf line (regex-replace-all (format nil "~R" i) (concatenate 'string line) (format nil "~R~D~R" i i i)))
						     finally (return line))))))
