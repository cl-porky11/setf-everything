(defpackage #:defaccessor
  (:use #:cl)
  (:export #:defaccessor
           #:define-method-accessor))
(in-package #:defaccessor)

(defpackage #:accessors
  (:export #:if
           #:let
           #:let*
           #:flet
           #:labels
           #:macrolet
           #:symbol-macrolet
           
           #:nthcdr
           #:last
           ))

(defmacro defun-alias (new old)
  `(setf (symbol-function ',new) (symbol-function ',old)))

#+nil
(defmacro defmacro-alias (new old)
  `(setf (macro-function ',new) (macro-function ',old)))

(defmacro defaccessor (name lambda-list &body body)
  "Define a new Function and it's setf automatically"
  (let ((value (gensym "VALUE")))
    `(progn
       (defun ,name ,lambda-list
         ,@body)
       (defun (setf ,name) ,(cons value lambda-list)
         ,@(butlast body)
         (setf ,@(last body) ,value)))))

(defmacro define-method-accessor (name lambda-list &body body)
  "Define a new Method and it's setf automatically"
  (let ((value (gensym "VALUE")))
    `(progn
       (defmethod ,name ,lambda-list
         ,@body)
       (defmethod (setf ,name) ,(cons value lambda-list)
         ,@(butlast body)
         (setf ,@(last body) ,value)))))


;;special-forms

#| following special forms are not setfable:
block return-from catch load-time-value tagbody function multiple-value-call go throw unwind-protect quote
if you have an idea, how to define them, do it |#

(defmacro accessors:if (test then &optional else)
  `(if ,test ,then ,else))

(define-setf-expander accessors:if (test then &optional else)
  (let ((value (gensym)))
    (values ()
            ()
            `(,value)
            `(if ,test
                 (setf ,then ,value)
                 ,(if else `(setf ,else ,value) value))
            `(if ,test ,then ,@(when else `(,else))))))

#+nil
(defmacro accessors:eval-when (any &body body)
  `(eval-when ,any ,@body))

#+nil
(define-setf-expander accessors:eval-when (&body body)
  (let ((value (gensym)))
    (values ()
            ()
            `(,value)
            `(eval-when
               ,@(butlast body)
               (setf ,@(last body) ,value))
            `(eval-when ,@body))))

#+nil
(define-setf-expander locally (&body body)
  (let ((value (gensym)))
    (values ()
            ()
            `(,value)
            `(locally
               ,@(butlast body)
               (setf ,@(last body) ,value))
            `(locally ,@body))))

#+nil
(define-setf-expander progv (&body body)
  (let ((value (gensym)))
    (values ()
            ()
            `(,value)
            `(eval-when
               ,@(butlast body)
               (setf ,@(last body) ,value))
            `(eval-when ,@body))))

#+nil
(define-setf-expander progn (&body body)
  (let ((value (gensym)))
    (values ()
            ()
            `(,value)
            `(progn ,@(butlast body)
                    (setf ,@(last body) ,value))
            `(progn ,@body))))

(defmacro accessors:let (bindings &body body)
  `(let ,bindings ,@body))

(define-setf-expander accessors:let (bindings &body body)
  (let ((value (gensym)))
    (values ()
            ()
            `(,value)
            `(let ,bindings
               ,@(butlast body)
               (setf ,@(last body) ,value))
            `(let ,bindings ,@body))))

(defmacro accessors:let* (bindings &body body)
  `(let* ,bindings ,@body))

(define-setf-expander accessors:let* (bindings &body body)
  (let ((value (gensym)))
    (values ()
            ()
            `(,value)
            `(let* ,bindings
               ,@(butlast body)
               (setf ,@(last body) ,value))
            `(let* ,bindings ,@body))))

(defmacro accessors:flet (definitions &body body)
  `(flet ,definitions ,@body))

(define-setf-expander accessors:flet (definitions &body body)
  (let ((value (gensym)))
    (values ()
            ()
            `(,value)
            `(flet ,definitions
               ,@(butlast body)
               (setf ,@(last body) ,value))
            `(flet ,definitions ,@body))))

(defmacro accessors:labels (definitions &body body)
  `(labels ,definitions ,@body))

(define-setf-expander accessors:labels (definitions &body body)
  (let ((value (gensym)))
    (values ()
            ()
            `(,value)
            `(labels ,definitions
               ,@(butlast body)
               (setf ,@(last body) ,value))
            `(labels ,definitions ,@body))))

(defmacro accessors:macrolet (definitions &body body)
  `(macrolet ,definitions ,@body))

(define-setf-expander accessors:macrolet (definitions &body body)
  (let ((value (gensym)))
    (values ()
            ()
            `(,value)
            `(macrolet ,definitions
               ,@(butlast body)
               (setf ,@(last body) ,value))
            `(macrolet ,definitions ,@body))))

(defmacro accessors:symbol-macrolet (macrobindings &body body)
  `(symbol-macrolet ,macrobindings ,@body))

(define-setf-expander accessors:symbol-macrolet (macrobindings &body body)
  (let ((value (gensym)))
    (values ()
            ()
            `(,value)
            `(symbol-macrolet ,macrobindings
               ,@(butlast body)
               (setf ,@(last body) ,value))
            `(symbol-macrolet ,macrobindings ,@body))))

#+nil
(define-setf-expander function (thing)
  (let ((value (gensym)))
    (values ()
            ()
            `(,value)
            `(let* ,bindings ,@(butlast body)
                    (setf ,@(last body) ,value))
            `(let* ,bindings ,@body))))

#+nil
(define-setf-expander multiple-value-prog1 (&body body)
  (let ((value (gensym)))
    (values ()
            ()
            `(,value)
            `(multiple-value-prog1
                 (setf ,(car body) ,value)
               ,@(cdr body))
            `(multiple-value-prog1 ,@body))))

#+nil
(define-setf-expander setq (&rest rest)
  (let ((value (gensym)))
    (values ()
            ()
            `(,value)
            `(progn
               (setq ,@rest)
               (setf ,@(last rest) ,value))
            `(setq ,@rest))))



;;functions

(defun-alias accessors:nthcdr nthcdr)

(define-setf-expander accessors:nthcdr (n list)
  (let ((value (gensym))
        (form `(nthcdr (1- ,n) ,list)))
    (values ()
            ()
            `(,value)
            `(setf (if (zerop ,n)
                       ,list
                       (cdr ,form))
                   ,value)
            `(nthcdr ,n ,list))))

(defun-alias accessors:last last)

(define-setf-expander accessors:last (list)
  (let ((value (gensym))
        (listsym (gensym)))
    (values ()
            ()
            `(,value)
            `(let ((,listsym ,list))
               (setf (nthcdr (1- (length ,listsym)) ,listsym) ,value))
            `(last ,list))))

