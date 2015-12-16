Most special forms and some functions in the package common-lisp are setfable now


There are also 2 macros in the package #:setf-everything

(defaccessor lambda-list &body function-body)
defines a function and the setf

(define-method-accessor specialized-lambda-list &body function-body)
defines a method and the setf

The new setfable function/method will setf the last expression in the body, if there are multiple expressions, so there will be no problem with documentations and declarations.
