;;; elimc.el --- Emacs lisp implementation of math calculate -*- lexical-binding: t -*-

;; Copyright (C) 2025 Matvii Jarosh

;; Author: Matvii Jarosh <matviijarosh@gmail.com>
;; Created: 2025-02-15
;; Version: 1.1

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
;; if you want plot call elimc-plot

;;; Code:

(defvar nan-value -0.0e+NaN)
(defvar expr-widget)
(defvar x-val-widget)
(defvar result-widget)
(defvar error-widget)
(defvar notation-widget)

(require 'widget)

;; utils
(defun elimc-token-operatorp (token)
  "Return t if TOKEN is a operator."
  (if (stringp token)
      (member token '("+" "-" "*" "/" "^"))
    (error "Token is not a string")))

(defun elimc-token-funp (token)
  "Return t if TOKEN is a math function."
  (if (stringp token)
      (member token '("sin" "cos" "tg" "ctg" "sqrt" "abs"
		      "arcsin" "arccos" "arctg"))
    (error "Token is not a string")))

(defun elimc-op-preced (token)
  "Return the operator precedence based on TOKEN."
  (if (stringp token)
      (cond
       ((string-equal token "^") 4)
       ((member token '("*" "/")) 3)
       ((member token '("+" "-")) 2)
       (t 0))
    (error "Token is not a string")))

(defun elimc-tokenize-math-expression (expr)
  "Splits a mathematical EXPR into a list of tokens."
  (if (stringp expr)
      (let ((expr (replace-regexp-in-string "\\([-+*/^()]\\)" " \\1 " expr)))
	(split-string (string-trim expr)))
    (error "Expr is not a string")))

;; shunting-yard
(defun elimc-shunting-yard (input)
  "Convert a mathematical expression to Reverse Polish Notation from INPUT."
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

;; rpnq
(defun elimc-rpn-fun (stack token)
  "Handles operations in the RPN.

STACK is a main RPN stack

TOKEN is a function

Returns the result of the operation, or NaN if invalid."
  (if (null stack)
      (list nan-value)
    (let* ((a (car stack))
           (new-stack (cdr stack)))
      (if (equal a nan-value)
          (cons nan-value new-stack)
        (let ((result (condition-case nil
                          (cond ((string-equal token "sin") (sin a))
                                ((string-equal token "cos") (cos a))
                                ((string-equal token "tg") (tan a))
                                ((string-equal token "ctg") (/ 1.0 (tan a)))
				((string-equal token "arcsin") (asin a))
				((string-equal token "arccos") (acos a))
				((string-equal token "arctg") (atan a))
                                ((string-equal token "sqrt")
                                 (if (< a 0) nan-value (sqrt a)))
                                ((string-equal token "abs") (abs a)))
                        (error nan-value))))
          (cons result new-stack))))))

(defun elimc-rpn-oper (a b token)
  "Handles operations in the RPN.

A and B is values from stack

TOKEN is a operator

Returns the result of the operation, or NaN if invalid."
  (if (or (equal a nan-value) (equal b nan-value))
      nan-value
    (condition-case nil
        (cond ((string-equal token "+") (+ a b))
              ((string-equal token "-") (- a b))
              ((string-equal token "*") (* a b))
              ((string-equal token "/") (if (zerop b) nan-value (/ a b)))
              ((string-equal token "^") (expt a b)))
      (error nan-value))))

(defun elimc-rpn (input x-val)
  "Calculates the value of an INPUT and X-VAL in RPN.

Return NaN if invalid."
  (if (and (listp input) (numberp x-val))
      (let ((tokens (copy-tree input))
            (stack nil))
        (while (and tokens (not (equal (car stack) nan-value)))
          (let ((token (pop tokens)))
            (condition-case nil
                (cond
                 ((string-match "^[0-9.]+$" token)
                  (push (float (string-to-number token)) stack))
                 ((elimc-token-funp token)
                  (setq stack (elimc-rpn-fun stack token)))
                 ((elimc-token-operatorp token)
                  (let ((b (pop stack))
                        (a (pop stack)))
                    (push (elimc-rpn-oper a b token) stack)))
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

(defmacro elimc-in-widget (buffer-name &rest body)
  "Macro for quickly building a widget base.

BUFFER-NAME is the name of the buffer in which the program will be executed.

BODY is the content of the application with all the code."
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

(defun elimc-calc-calculate (expr notation x-val result-widget-local error-widget-local)
  "Calculate the RESULT based on the EXPR, NOTATION, and X-VAL.
Then update the RESULT-WIDGET-LOCAL and ERROR-WIDGET-LOCAL."
  (condition-case err
      (let* ((tokens (elimc-tokenize-math-expression expr))
             (result (if (string= notation "Infix")
                         (elimc-rpn (elimc-shunting-yard tokens) x-val)
                       (elimc-rpn tokens x-val))))
        (widget-value-set result-widget-local (format "%s" result))
        (widget-value-set error-widget-local ""))
    (error
     (widget-value-set result-widget-local "")
     (widget-value-set error-widget-local (error-message-string err)))))


(defun elimc-calc ()
  "Client for calculating values."
  (interactive)
  (elimc-in-widget "*RPN calculator*"
		   (setq expr-widget
			 (widget-create 'editable-field
					:format "RPN expr:\n%v"
					"2 + 2"))
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
				    (ignore ignore)
				    (elimc-calc-calculate
				     (widget-value expr-widget)
				     (widget-value notation-widget)
				     (string-to-number (widget-value x-val-widget))
				     result-widget
				     error-widget))
				  "Calculate")))

;; ui graphic
(defun elimc-create-image (width height)
  "Create an XPM image as a list, filling it with white color.

WIDTH and HEIGHT this is the size of the image."
  (let ((default-color " "))
    (make-list height (make-list width default-color))))

(defun elimc-fill-image (image color)
  "Fills all pixels of the image IMAGE with the specified COLOR."
  (mapcar (lambda (row) (make-list (length row) color)) image))

(defun elimc-setpixel (image x y color)
  "Set the pixel in IMAGE at position (X, Y) to the specified COLOR."
  (setf (nth y image)
        (append (cl-subseq (nth y image) 0 x)
                (list color)
                (cl-subseq (nth y image) (1+ x)))))

(defun elimc-image-to-xpm (image)
  "Convert IMAGE to XPM string representation."
  (let* ((width (length (car image)))
         (height (length image))
         (color-map (make-hash-table :test 'equal))
         (colors '())
         (pixels '())
         (char-list
	  "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789"))
    (dolist (row image)
      (dolist (pixel row)
        (unless (gethash pixel color-map)
          (let ((char (string (aref char-list (length colors)))))
            (push (format "%s c %s" char pixel) colors)
            (puthash pixel char color-map)))))
    (dolist (row image)
      (push (mapconcat (lambda (pixel) (gethash pixel color-map)) row "") pixels))
    (concat "/* XPM */\nstatic char *image_xpm[] = {\n"
            (format "\"%d %d %d 1\",\n" width height (length colors))
            (mapconcat (lambda (c) (concat "\"" c "\",")) (reverse colors) "\n")
            "\n"
            (mapconcat (lambda (p) (concat "\"" p "\",")) (reverse pixels) "\n")
            "};")))

(defun elimc--push-image (image)
  "Create a new buffer and insert the IMAGE into it."
  (with-help-window "*Plot image*"
    (with-current-buffer "*Plot image*"
      (insert-image image))))

(defun elimc-plot-create-graph (expr notation error-widget-local)
  "Create a plot for the given EXPR in the specified NOTATION.
Then update the ERROR-WIDGET-LOCAL."
  (condition-case err
      (let*
          ((tokens (elimc-tokenize-math-expression expr))
           (rpn (if (string= notation "Infix")
                    (elimc-shunting-yard tokens)
                  tokens))
           (image (elimc-fill-image (elimc-create-image 400 400) "#ffffff")))

        ;; grid
        (dotimes (x 400)
          (dotimes (y 400)
            (when (or (= (mod x 50) 0)
                      (= (mod y 50) 0))
              (elimc-setpixel image x y "#cccccc"))))

        ;; plot
	(dotimes (x 400)
	  (let* ((x-val (- (/ (* x 8.0) 400) 4))
		 (y-val (elimc-rpn rpn x-val)))
	    (if (or (isnan y-val) (> (abs y-val) 1.0e6))
		(setq y-val 0.0e+NaN)
	      (let ((y (round (- 200 (* 50 y-val)))))
		(when (and (>= y 0) (< y 400))
		  (elimc-setpixel image x y "#ff0000")
		  (when (> x 0)
		    (elimc-setpixel image (1- x) y "#ff0000"))
		  (when (< x 399)
		    (elimc-setpixel image (1+ x) y "#ff0000"))
		  (when (> y 0)
		    (elimc-setpixel image x (1- y) "#ff0000"))
		  (when (< y 399)
		    (elimc-setpixel image x (1+ y) "#ff0000")))))))


        ;; display img
        (let ((pbm (elimc-image-to-xpm image)))
          (with-current-buffer "*Function Plotter*"
            (let ((inhibit-read-only t))
              (goto-char (point-max))
              (elimc--push-image (create-image pbm 'xpm ""))))))
    (error
     (widget-value-set error-widget-local (error-message-string err)))))

(defun elimc-plot ()
  "Client for drawing plot mathematics."
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
				    (ignore ignore)
				    (elimc-plot-create-graph
				     (widget-value expr-widget)
				     (widget-value notation-widget)
				     error-widget))
				  "Plot")))

;;; elimc.el ends here
