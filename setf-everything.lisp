(defpackage #:setf-everything
  (:use #:cl)
  (:export #:defaccessor
           #:define-method-accessor))
(in-package #:setf-everything)

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


(in-package :cl)

;;special-forms

#| following special forms are not setfable:
block return-from catch load-time-value tagbody function multiple-value-call go throw unwind-protect quote
if you have an idea, how to define them, do it |#

(define-setf-expander if (test then &optional else)
  (let ((value (gensym)))
    (values ()
            ()
            `(,value)
            `(if ,test
                 (setf ,then ,value)
                 ,(if else `(setf ,else ,value) value))
            `(if ,test ,then ,@(when else `(,else))))))

(define-setf-expander eval-when (&body body)
  (let ((value (gensym)))
    (values ()
            ()
            `(,value)
            `(eval-when
               ,@(butlast body)
               (setf ,@(last body) ,value))
            `(eval-when ,@body))))

(define-setf-expander locally (&body body)
  (let ((value (gensym)))
    (values ()
            ()
            `(,value)
            `(locally
               ,@(butlast body)
               (setf ,@(last body) ,value))
            `(locally ,@body))))

(define-setf-expander progv (&body body)
  (let ((value (gensym)))
    (values ()
            ()
            `(,value)
            `(eval-when
               ,@(butlast body)
               (setf ,@(last body) ,value))
            `(eval-when ,@body))))

(define-setf-expander progn (&body body)
  (let ((value (gensym)))
    (values ()
            ()
            `(,value)
            `(progn ,@(butlast body)
                    (setf ,@(last body) ,value))
            `(progn ,@body))))

(define-setf-expander let (&body body)
  (let ((value (gensym)))
    (values ()
            ()
            `(,value)
            `(let ,@(butlast body)
                    (setf ,@(last body) ,value))
            `(let ,@body))))

(define-setf-expander let* (&body body)
  (let ((value (gensym)))
    (values ()
            ()
            `(,value)
            `(let* ,@(butlast body)
                    (setf ,@(last body) ,value))
            `(let* ,@body))))

(define-setf-expander flet (&body body)
  (let ((value (gensym)))
    (values ()
            ()
            `(,value)
            `(flet ,@(butlast body)
                    (setf ,@(last body) ,value))
            `(flet ,@body))))

(define-setf-expander labels (&body body)
  (let ((value (gensym)))
    (values ()
            ()
            `(,value)
            `(labels ,@(butlast body)
                    (setf ,@(last body) ,value))
            `(labels ,@body))))

(define-setf-expander macrolet (&body body)
  (let ((value (gensym)))
    (values ()
            ()
            `(,value)
            `(macrolet ,@(butlast body)
                    (setf ,@(last body) ,value))
            `(macrolet ,@body))))

(define-setf-expander symbol-macrolet (&body body)
  (let ((value (gensym)))
    (values ()
            ()
            `(,value)
            `(symbol-macrolet ,@(butlast body)
                    (setf ,@(last body) ,value))
            `(symbol-macrolet ,@body))))

(define-setf-expander function (thing)
  (let ((value (gensym)))
    (values ()
            ()
            `(,value)
            `(let* ,bindings ,@(butlast body)
                    (setf ,@(last body) ,value))
            `(let* ,bindings ,@body))))

(define-setf-expander multiple-value-prog1 (&body body)
  (let ((value (gensym)))
    (values ()
            ()
            `(,value)
            `(multiple-value-prog1
                 (setf ,(car body) ,value)
               ,@(cdr body))
            `(multiple-value-prog1 ,@body))))

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

(define-setf-expander nthcdr (n list)
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

(define-setf-expander last (list)
  (let ((value (gensym))
        (listsym (gensym)))
    (values ()
            ()
            `(,value)
            `(let ((,listsym ,list))
               (setf (nthcdr (1- (length ,listsym)) ,listsym) ,value))
            `(last ,list))))

