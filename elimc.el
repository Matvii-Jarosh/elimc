;;; elimc.el --- Emacs lisp implementation of math calculate

;; Copyright (C) 2025 Matvii Jarosh

;; Author: Matvii Jarosh <matviijarosh@gmail.com>
;; Created: 2025-02-15

;; elimc is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; elimc is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with elimc.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; This is a simple TUI application for calculating
;; mathematical problems using infix and rpn

;; to get started with simple calculator call elimc-calc
;; if you want plot call elimc-plo

;;; Code:

(require 'widget)

;; utils
(defun elimc-token-operatorp (token)
  "Return t if token is a operator"
  (if (stringp token)
      (member token '("+" "-" "*" "/" "^"))
    (error "Token is not a string")))

(defun elimc-token-funp (token)
  "Return t if token is a math function"
  (if (stringp token)
      (member token '("sin" "cos" "tg" "ctg" "sqrt" "abs"))
    (error "Token is not a string")))

(defun elimc-op-preced (token)
  "Returns the operator precedence"
  (if (stringp token)
      (cond
       ((string-equal token "^") 4)
       ((member token '("*" "/")) 3)
       ((member token '("+" "-")) 2)
       (t 0))
    (error "Token is not a string")))

(defun elimc-tokenize-math-expression (expr)
  "Splits a mathematical expression into a list of tokens."
  (if (stringp expr)
      (let ((expr (replace-regexp-in-string "\\([-+*/^()]\\)" " \\1 " expr)))
	(split-string (string-trim expr)))
    (error "Expr is not a string")))

;; shunting-yard 
(defun elimc-shunting-yard (input)
  "Converts a mathematical expression to Reverse Polish Notation (RPN)"
  (if (listp input)
      (let ((tokens (copy-tree input))
            (stack nil)
            (output nil))
	(while tokens
	  (let ((token (pop tokens)))
            (cond
             ((string-match "^[0-9.]+$" token) ;; is number
              (push token output)) 
             ((elimc-token-funp token) ;; is function
              (push token stack))
             ((elimc-token-operatorp token) ;; is operator
              (while (and stack
			  (not (string-equal (car stack) "("))
			  (<= (elimc-op-preced token)
			      (elimc-op-preced (car stack))))
		(push (pop stack) output))
              (push token stack))
	     ((member token '("x" "pi" "e")) ;; is x or Pi or E
	      (push token output))
             ((string-equal token "(") ;; is "("
              (push token stack))
             ((string-equal token ")") ;; is ")"
              (while (and stack (not (string-equal (car stack) "(")))
		(push (pop stack) output))
              (pop stack)
              (when (and stack (elimc-token-funp (car stack)))
		(push (pop stack) output)))
	     (t ;; Error
	      (error "Invalid token")))))

	(while stack
	  (push (pop stack) output))

	(nreverse output))
    (error "The input argument is not a list")))

;; rpn
(defun elimc-rpn (input x-val)
  "Calculates the value of an expression in RPN. Returns NaN if invalid."
  (if (and (listp input) (numberp x-val))
      (let ((tokens (copy-tree input))
            (stack nil)
            (nan-value 0.0e+NaN))
        (while (and tokens (not (equal (car stack) nan-value)))
          (let ((token (pop tokens)))
            (condition-case err
                (cond
                 ((string-match "^[0-9.]+$" token)
                  (push (float (string-to-number token)) stack))
                 ((elimc-token-funp token)
                  (let ((a (pop stack)))
                    (if (equal a nan-value)
                        (push nan-value stack)
                      (let ((result (condition-case nil
                                        (cond ((string-equal token "sin") (sin a))
                                              ((string-equal token "cos") (cos a))
                                              ((string-equal token "tg") (tan a))
                                              ((string-equal token "ctg") (/ 1.0 (tan a)))
                                              ((string-equal token "sqrt")
                                               (if (< a 0) nan-value (sqrt a)))
                                              ((string-equal token "abs") (abs a)))
                                      (error nan-value))))
                        (push result stack)))))
                 ((elimc-token-operatorp token)
                  (let ((b (pop stack))
                        (a (pop stack)))
                    (if (or (equal a nan-value) (equal b nan-value))
                        (push nan-value stack)
                      (let ((result (condition-case nil
                                        (cond ((string-equal token "+") (+ a b))
                                              ((string-equal token "-") (- a b))
                                              ((string-equal token "*") (* a b))
                                              ((string-equal token "/")
                                               (if (zerop b) nan-value (/ a b)))
                                              ((string-equal token "^") (expt a b)))
                                      (error nan-value))))
                        (push result stack)))))
                 ((member token '("x" "pi" "e"))
                  (push (float (cond ((string-equal token "x") x-val)
                                     ((string-equal token "pi") float-pi)
                                     ((string-equal token "e") float-e)))
                        stack)))
              (error (push nan-value stack)))))
        (if (or (null stack) (equal (car stack) nan-value))
            nan-value
          (pop stack)))
    (error "Invalid input or x-val")))



;; tui
(eval-when-compile
  (require 'wid-edit))

(defvar expr-widget)
(defvar x-val-widget)
(defvar result-widget)
(defvar error-widget)
(defvar notation-widget)

(defmacro elimc-in-widget (buffer-name &rest body)
  `(progn
     (switch-to-buffer ,buffer-name)
     (kill-all-local-variables)
     (make-local-variable 'expr-widget)
     (make-local-variable 'x-val-widget)
     (make-local-variable 'result-widget)
     (make-local-variable 'error-widget)
     (make-local-variable 'notation-widget)
     (let ((inhibit-read-only t))
       (erase-buffer))
     (remove-overlays)
     ,@body
     (use-local-map widget-keymap)
     (widget-setup)))

(defun elimc-calc ()
  "Client for calculating values "
  (interactive)
  (elimc-in-widget "*RPN calculator*"
		   (setq expr-widget
			 (widget-create 'editable-field
					:format "RPN expr:\n%v"
					"2 2 +"))
		   (widget-insert "\n")
		   (setq x-val-widget
			 (widget-create 'editable-field
					:format "x value:%v"
					"5"))
		   (widget-insert "\n")
		   (widget-insert "Result: ")
		   (setq result-widget (widget-create 'item :value ""))
		   (widget-insert "\n")
		   (widget-insert "Error: ")
		   (setq error-widget (widget-create 'item :value ""))
		   (widget-insert "\n")
		   (setq notation-widget (widget-create 'radio-button-choice
				  :value "Infix"
				  '(item "Infix") '(item "RPN")))
		   (widget-create 'push-button
				  :notify
				  (lambda (&rest ignore)
				    (condition-case err
					(let*
					    ((expr
					      (widget-value
					       expr-widget))
					     (notation
					      (widget-value notation-widget))
					     (tokens
					      (elimc-tokenize-math-expression
					       expr))
					     (x-val
					      (string-to-number
					       (widget-value x-val-widget)))
					     (result
					      (if
						  (string= notation "Infix")
                                                  (elimc-rpn
						   (elimc-shunting-yard tokens)
						   x-val)
                                                (elimc-rpn tokens x-val))))
					  (widget-value-set
					   result-widget
					   (format "%s" result))
					  (widget-value-set error-widget ""))
				      (error 
				       (widget-value-set result-widget "")
				       (widget-value-set
					error-widget
					(error-message-string err))))
				    (widget-setup))
				  "Calculate")))

;; ui graphic
(defun elimc-create-image (width height)
  "Creates an image as a list, filling it with white (255)"
  (let ((default-color 255))
    (make-list height (make-list width default-color))))

(defun elimc-fill-image (image color)
  "Fills all pixels of the image IMAGE with the specified color COLOR"
  (mapcar (lambda (row) (make-list (length row) color)) image))

(defun elimc-setpixel (image x y color)
  "Sets the pixel in the image IMAGE at position (x, y) to the color COLOR"
  (setf (nth x (nth y image)) color))

(defun elimc-image-to-pbm (image)
  "Converts an IMAGE image to a PBM string representation"
  (let* ((width (length (car image)))
         (height (length image))
         (pbm-data (list (format "%d %d" width height)
			 "P1")))
    (dolist (row image)
      (push (mapconcat
	     (lambda (pixel)
	       (if (= pixel 0) "1" "0"))
	     row " ")
	    pbm-data))
    (string-join (reverse pbm-data) "\n")))

(defun elimc--push-image (image)
  "Creates a new buffer and inserts the image into it"
  (with-help-window "*Plot image*"
    (with-current-buffer "*Plot image*"
      (insert-image image))))

(defun elimc-plot ()
  "Client for drawing plot mathematics"
  (interactive)
  (elimc-in-widget "*Function Plotter*"
		   (setq expr-widget
			 (widget-create 'editable-field
					:format "Function (infix notation):\n%v"
					"x^2"))
		   (widget-insert "\n")
		   (widget-insert "Error: ")
		   (setq error-widget (widget-create 'item :value ""))
		   (widget-insert "\n")
		   (setq notation-widget (widget-create 'radio-button-choice
				  :value "Infix"
				  '(item "Infix") '(item "RPN")))
		   (widget-insert "\n")
		   (widget-create 'push-button
				  :notify
				  (lambda (&rest ignore)
				    (condition-case err
					(let*
					    ((expr (widget-value expr-widget))
					     (notation
					      (widget-value notation-widget))
					     (tokens
					      (elimc-tokenize-math-expression
					       expr))
					     (rpn
					      (if
						  (string= notation "Infix")                                                  
						  (elimc-shunting-yard tokens)
                                                 tokens)) 
					     (image
					      (elimc-fill-image
					       (elimc-create-image 400 400)
					       0)))

					  ;; grid
					  (dotimes (x 400)
					    (dotimes (y 400)
					      (when (or (= (mod x 50) 0)
							(= (mod y 50) 0))
						(elimc-setpixel image x y 1))))
					  
					  ;; plot
					  (dotimes (x 400)
					    (let* ((x-val
						    (- (/ (* x 8.0) 400) 4))
						   (y-val
						    (elimc-rpn rpn x-val)))
					      (if (or (isnan y-val)
						      (> (abs y-val) 1.0e6))
						  (setq y-val 0.0e+NaN)
						(let ((y (round
							  (- 200
							     (* 50 y-val)))))
						  (when (and (>= y 0)
							     (< y 400))
						    (elimc-setpixel
						     image x y 1))))))
					  
					  ;; display img
					  (let
					      ((pbm
						(elimc-image-to-pbm image)))
					    (with-current-buffer 
						"*Function Plotter*"
					      (let ((inhibit-read-only t))
						(goto-char (point-max))
						(elimc--push-image
						 (create-image
						  pbm 'pbm ""))))))
				      (error 
				       (widget-value-set
					error-widget
					(error-message-string err)))))
				  "Plot")))

;;; elimc.el ends here
