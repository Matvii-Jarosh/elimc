;;; elimc.el --- Emacs lisp implementation of math calculate -*- lexical-binding: t -*-

;; Copyright (C) 2025 Matvii Jarosh

;; Author: Matvii Jarosh <matviijarosh@gmail.com>
;; Created: 2025-02-15
;; Version: 1.2

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

(defvar elimc-nan-value -0.0e+NaN)
(defvar elimc-expr-widget)
(defvar elimc-x-val-widget)
(defvar elimc-result-widget)
(defvar elimc-error-widget)
(defvar notation-widget)

(require 'widget)

;; utils
(defun elimc-token-operatorp
    (token)
  "Return t if TOKEN is a operator."
  (if
      (stringp token)
      (member token
	      '("+" "-" "*" "/" "^"))
    (error "Token is not a string")))

(defun elimc-token-funp
    (token)
  "Return t if TOKEN is a math function."
  (if
      (stringp token)
      (member token
	      '("sin" "cos" "tg" "ctg" "sqrt" "abs" "arcsin" "arccos" "arctg"))
    (error "Token is not a string")))

(defun elimc-op-preced
    (token)
  "Return the operator precedence based on TOKEN."
  (if
      (stringp token)
      (cond
       ((string-equal token "^")
	4)
       ((member token
		'("*" "/"))
	3)
       ((member token
		'("+" "-"))
	2)
       (t 0))
    (error "Token is not a string")))

(defun elimc-tokenize-math-expression
    (expr)
  "Splits a mathematical EXPR into a list of tokens."
  (if
      (stringp expr)
      (let
	  ((expr
	    (replace-regexp-in-string "\\([-+*/^()]\\)" " \\1 " expr)))
	(split-string
	 (string-trim expr)))
    (error "Expr is not a string")))

;; shunting-yard
(defun elimc-shunting-yard
    (input)
  "Convert a mathematical expression to Reverse Polish Notation from INPUT."
  (if
      (listp input)
      (let
	  ((tokens
	    (copy-tree input))
	   (stack nil)
	   (output nil))
	(while tokens
	  (let
	      ((token
		(pop tokens)))
	    (cond
	     ((string-match "^[0-9.]+$" token)
	      (push token output))
	     ((elimc-token-funp token)
	      (push token stack))
	     ((elimc-token-operatorp token)
	      (while
		  (and stack
		       (not
			(string-equal
			 (car stack)
			 "("))
		       (<=
			(elimc-op-preced token)
			(elimc-op-preced
			 (car stack))))
		(push
		 (pop stack)
		 output))
	      (push token stack))
	     ((member token
		      '("x" "pi" "e"))
	      (push token output))
	     ((string-equal token "(")
	      (push token stack))
	     ((string-equal token ")")
	      (while
		  (and stack
		       (not
			(string-equal
			 (car stack)
			 "(")))
		(push
		 (pop stack)
		 output))
	      (pop stack)
	      (when
		  (and stack
		       (elimc-token-funp
			(car stack)))
		(push
		 (pop stack)
		 output)))
	     (t
	      (error "Invalid token")))))
	(while stack
	  (push
	   (pop stack)
	   output))
	(nreverse output))
    (error "The input argument is not a list")))

;; rpnq
(defun elimc-rpn-fun
    (stack token)
  "Handles operations in the RPN.\n\n STACK is a main RPN stack\n\n TOKEN is a function\n\nReturns the result of the operation, or NaN if invalid."
  (if
      (null stack)
      (list elimc-nan-value)
    (let*
	((a
	  (car stack))
	 (new-stack
	  (cdr stack)))
      (if
	  (equal a elimc-nan-value)
	  (cons elimc-nan-value new-stack)
	(let
	    ((result
	      (condition-case nil
		  (cond
		   ((string-equal token "sin")
		    (sin a))
		   ((string-equal token "cos")
		    (cos a))
		   ((string-equal token "tg")
		    (tan a))
		   ((string-equal token "ctg")
		    (/ 1.0
		       (tan a)))
		   ((string-equal token "arcsin")
		    (asin a))
		   ((string-equal token "arccos")
		    (acos a))
		   ((string-equal token "arctg")
		    (atan a))
		   ((string-equal token "sqrt")
		    (if
			(< a 0)
			elimc-nan-value
		      (sqrt a)))
		   ((string-equal token "abs")
		    (abs a)))
		(error elimc-nan-value))))
	  (cons result new-stack))))))

(defun elimc-rpn-oper
    (a b token)
  "Handles operations in the RPN.\n\n A and B is values from stack\n\n TOKEN is a operator\n\nReturns the result of the operation, or NaN if invalid."
  (if
      (or
       (equal a elimc-nan-value)
       (equal b elimc-nan-value))
      elimc-nan-value
    (condition-case nil
	(cond
	 ((string-equal token "+")
	  (+ a b))
	 ((string-equal token "-")
	  (- a b))
	 ((string-equal token "*")
	  (* a b))
	 ((string-equal token "/")
	  (if
	      (zerop b)
	      elimc-nan-value
	    (/ a b)))
	 ((string-equal token "^")
	  (expt a b)))
      (error elimc-nan-value))))

(defun elimc-rpn-const
    (stack token x-val)
  "Push const (or X-VAL) to STACK based on TOKEN."
  (push
   (float
    (cond
     ((string-equal token "x")
      x-val)
     ((string-equal token "pi")
      float-pi)
     ((string-equal token "e")
      float-e)))
   stack)
  stack)

(defun elimc-rpn
    (input x-val)
  "Calculates the value of an INPUT and X-VAL in RPN.\n\nReturn NaN if invalid."
  (if
      (and
       (listp input)
       (numberp x-val))
      (let
	  ((tokens
	    (copy-tree input))
	   (stack nil))
	(while
	    (and tokens
		 (not
		  (equal
		   (car stack)
		   elimc-nan-value)))
	  (let
	      ((token
		(pop tokens)))
	    (condition-case nil
		(cond
		 ((string-match "^[0-9.]+$" token)
		  (push
		   (float
		    (string-to-number token))
		   stack))
		 ((elimc-token-funp token)
		  (setq stack
			(elimc-rpn-fun stack token)))
		 ((elimc-token-operatorp token)
		  (let
		      ((b
			(pop stack))
		       (a
			(pop stack)))
		    (push
		     (elimc-rpn-oper a b token)
		     stack)))
		 ((member token
			  '("x" "pi" "e"))
		  (setq stack
			(elimc-rpn-const stack token x-val))))
	      (error
	       (push elimc-nan-value stack)))))
	(if
	    (or
	     (null stack)
	     (equal
	      (car stack)
	      elimc-nan-value))
	    elimc-nan-value
	  (pop stack)))
    (error "Invalid input or x-val")))

;; tui
(eval-when-compile
  (require 'wid-edit))

(defmacro elimc-in-widget
    (buffer-name &rest body)
  "Macro for quickly building a widget base.\n\n BUFFER-NAME is the name of the buffer in which the program will be executed.\n\n BODY is the content of the application with all the code."
  `(progn
     (switch-to-buffer ,buffer-name)
     (kill-all-local-variables)
     (make-local-variable 'elimc-expr-widget)
     (make-local-variable 'elimc-x-val-widget)
     (make-local-variable 'elimc-result-widget)
     (make-local-variable 'elimc-error-widget)
     (make-local-variable 'elimc-notation-widget)
     (let
	 ((inhibit-read-only t))
       (erase-buffer))
     (remove-overlays)
     ,@body
     (use-local-map widget-keymap)
     (widget-setup)))

(defun elimc-calc-calculate
    (expr notation x-val elimc-result-widget-local elimc-error-widget-local)
  "Calculate the RESULT based on the EXPR, NOTATION, and X-VAL.\nThen update the ELIMC-RESULT-WIDGET-LOCAL and ELIMC-ERROR-WIDGET-LOCAL."
  (condition-case err
      (let*
	  ((tokens
	    (elimc-tokenize-math-expression expr))
	   (result
	    (if
		(string= notation "Infix")
		(elimc-rpn
		 (elimc-shunting-yard tokens)
		 x-val)
	      (elimc-rpn tokens x-val))))
	(widget-value-set elimc-result-widget-local
			  (format "%s" result))
	(widget-value-set elimc-error-widget-local ""))
    (error
     (widget-value-set elimc-result-widget-local "")
     (widget-value-set elimc-error-widget-local
		       (error-message-string err)))))

(defun elimc-calc nil "Client for calculating values."
       (interactive)
       (elimc-in-widget "*RPN calculator*"
			(setq elimc-expr-widget
			      (widget-create 'editable-field :format "RPN expr:\n%v" "2 + 2"))
			(widget-insert "\n")
			(setq elimc-x-val-widget
			      (widget-create 'editable-field :format "x value:%v" "5"))
			(widget-insert "\n")
			(widget-insert "Result: ")
			(setq elimc-result-widget
			      (widget-create 'item :value ""))
			(widget-insert "\n")
			(widget-insert "Error: ")
			(setq elimc-error-widget
			      (widget-create 'item :value ""))
			(widget-insert "\n")
			(setq elimc-notation-widget
			      (widget-create 'radio-button-choice :value "Infix"
					     '(item "Infix")
					     '(item "RPN")))
			(widget-create 'push-button :notify
				       (lambda
					 (&rest ignore)
					 (ignore ignore)
					 (elimc-calc-calculate
					  (widget-value elimc-expr-widget)
					  (widget-value elimc-notation-widget)
					  (string-to-number
					   (widget-value elimc-x-val-widget))
					  elimc-result-widget elimc-error-widget))
				       "Calculate")))

;; ui graphic
(defun elimc-create-image
    (width height)
  "Create an XPM image as a list, filling it with white color.\n\n WIDTH and HEIGHT this is the size of the image."
  (let
      ((default-color " "))
    (make-list height
	       (make-list width default-color))))

(defun elimc-fill-image
    (image color)
  "Fills all pixels of the image IMAGE with the specified COLOR."
  (mapcar
   (lambda
     (row)
     (make-list
      (length row)
      color))
   image))

(defun elimc-setpixel
    (image x y color)
  "Set the pixel in IMAGE at position (X, Y) to the specified COLOR."
  (setf
   (nth y image)
   (append
    (cl-subseq
     (nth y image)
     0 x)
    (list color)
    (cl-subseq
     (nth y image)
     (1+ x)))))

(defun elimc-generate-color-map
    (image)
  "Create a table of color-symbol mappings from IMAGE."
  (let
      ((color-map
	(make-hash-table :test 'equal))
       (colors 'nil)
       (char-list "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789"))
    (dolist
	(row image)
      (dolist
	  (pixel row)
	(unless
	    (gethash pixel color-map)
	  (let
	      ((char
		(string
		 (aref char-list
		       (length colors)))))
	    (push
	     (format "%s c %s" char pixel)
	     colors)
	    (puthash pixel char color-map)))))
    (cons color-map
	  (reverse colors))))

(defun elimc-generate-pixel-rows
    (image color-map)
  "Generate pixel rows for XPM from IMAGE and COLOR-MAP."
  (let
      ((pixels 'nil))
    (dolist
	(row image)
      (push
       (mapconcat
	(lambda
	  (pixel)
	  (gethash pixel color-map))
	row "")
       pixels))
    (reverse pixels)))

(defun elimc-generate-xpm-header
    (width height color-count)
  "Create an XPM header from WIDTH HEIGHT COLOR-COUNT."
  (format "/* XPM */\nstatic char *image_xpm[] = {\n\"%d %d %d 1\",\n" width height color-count))

(defun elimc-image-to-xpm
    (image)
  "Convert IMAGE to XPM string."
  (let*
      ((width
	(length
	 (car image)))
       (height
	(length image))
       (color-data
	(elimc-generate-color-map image))
       (color-map
	(car color-data))
       (colors
	(cdr color-data))
       (pixels
	(elimc-generate-pixel-rows image color-map)))
    (concat
     (elimc-generate-xpm-header width height
				(length colors))
     (mapconcat
      (lambda
	(c)
	(concat "\"" c "\","))
      colors "\n")
     "\n"
     (mapconcat
      (lambda
	(p)
	(concat "\"" p "\","))
      pixels "\n")
     "};")))

(defun elimc--push-image
    (image)
  "Create a new buffer and insert the IMAGE into it."
  (with-help-window "*Plot image*"
    (with-current-buffer "*Plot image*"
      (insert-image image))))

(defun elimc-plot-generate-grid
    (image)
  "Draws a gray grid on the IMAGE."
  (dotimes
      (x 400)
    (dotimes
	(y 400)
      (when
	  (or
	   (=
	    (mod x 50)
	    0)
	   (=
	    (mod y 50)
	    0))
	(elimc-setpixel image x y "#cccccc")))))

(defun elimc-plot-evaluate-expression
    (rpn x-val)
  "Calculates the value of an expression in RPN for the X-VAL."
  (let
      ((y-val
	(elimc-rpn rpn x-val)))
    (if
	(or
	 (isnan y-val)
	 (>
	  (abs y-val)
	  1000000.0))
	0.0e+NaN y-val)))

(defun elimc-plot-draw-function
    (image rpn)
  "Draws a graph of a function on an IMAGE from RPN."
  (dotimes
      (x 400)
    (let*
	((x-val
	  (-
	   (/
	    (* x 8.0)
	    400)
	   4))
	 (y-val
	  (elimc-plot-evaluate-expression rpn x-val)))
      (unless
	  (isnan y-val)
	(let
	    ((y
	      (round
	       (- 200
		  (* 50 y-val)))))
	  (when
	      (and
	       (>= y 0)
	       (< y 400))
	    (elimc-setpixel image x y "#ff0000")
	    (when
		(> x 0)
	      (elimc-setpixel image
			      (1- x)
			      y "#ff0000"))
	    (when
		(< x 399)
	      (elimc-setpixel image
			      (1+ x)
			      y "#ff0000"))
	    (when
		(> y 0)
	      (elimc-setpixel image x
			      (1- y)
			      "#ff0000"))
	    (when
		(< y 399)
	      (elimc-setpixel image x
			      (1+ y)
			      "#ff0000"))))))))

(defun elimc-plot-display
    (image)
  "Displays an IMAGE in the buffer."
  (let
      ((pbm
	(elimc-image-to-xpm image)))
    (with-current-buffer "*Function Plotter*"
      (let
	  ((inhibit-read-only t))
	(goto-char
	 (point-max))
	(elimc--push-image
	 (create-image pbm 'xpm ""))))))

(defun elimc-plot-create-graph
    (expr notation elimc-error-widget-local)
  "Create a plot for the given EXPR in the specified NOTATION.\nThen update the ELIMC-ERROR-WIDGET-LOCAL."
  (condition-case err
      (let*
	  ((tokens
	    (elimc-tokenize-math-expression expr))
	   (rpn
	    (if
		(string= notation "Infix")
		(elimc-shunting-yard tokens)
	      tokens))
	   (image
	    (elimc-fill-image
	     (elimc-create-image 400 400)
	     "#ffffff")))
	(elimc-plot-generate-grid image)
	(elimc-plot-draw-function image rpn)
	(elimc-plot-display image))
    (error
     (widget-value-set elimc-error-widget-local
		       (error-message-string err)))))

(defun elimc-plot nil "Client for drawing plot mathematics."
       (interactive)
       (elimc-in-widget "*Function Plotter*"
			(setq elimc-expr-widget
			      (widget-create 'editable-field :format "Function (infix notation):\n%v" "x^2"))
			(widget-insert "\n")
			(widget-insert "Error: ")
			(setq elimc-error-widget
			      (widget-create 'item :value ""))
			(widget-insert "\n")
			(setq elimc-notation-widget
			      (widget-create 'radio-button-choice :value "Infix"
					     '(item "Infix")
					     '(item "RPN")))
			(widget-insert "\n")
			(widget-create 'push-button :notify
				       (lambda
					 (&rest ignore)
					 (ignore ignore)
					 (elimc-plot-create-graph
					  (widget-value elimc-expr-widget)
					  (widget-value elimc-notation-widget)
					  elimc-error-widget))
				       "Plot")))

;;; elimc.el ends here
