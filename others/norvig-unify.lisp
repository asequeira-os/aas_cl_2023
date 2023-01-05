;; safe keeping some code I found on Norvig paper

;; package stuff is mine - antony
;; rest of code is from http://norvig.com/unify-bug.pdf as of 2010/10/29

(defpackage :norvig-unify
  (:use :cl))

(in-package :norvig-unify)

;; Peter Norvig code follows
(defun unify (x y &optional subst)
  (cond ((equal x y) subst)
        ((equal subst 'fail) 'fail)
        ((var? x) (unify-variable x y subst))
        ((var? y) (unify-variable y x subst))
        ((or (atom x) (atom y)) 'fail)
        (t (unify (rest x) (rest y)
                  (unify (first x) (first y) subst)))))

;;antony - I renamed this from unify-variable since paper provides the
;;buggy and fixed versions
(defun buggy-unify-variable (var val subst)
  "Unify var with val, using (and possibly extending) subst."
  (cond ((equal var val) subst)
        ((bound? var subst)
         (unify (lookup var subst) val subst))
        ((occurs-in? var val subst) 'fail)
        (t (extend-subst var val subst))))

(defun occurs-in? (var x subst)
  "Does var occur anywhere inside x?"
  (cond ((equal var x) t)
        ((bound? x subst)
         (occurs-in? var (lookup x subst) subst))
        ((consp x) (or (occurs-in? var (first x) subst)
                       (occurs-in? var (rest x) subst)))
        (t nil)))

(defun unify-variable (var val subst)
  "Unify var with val, using (and possibly extending) subst."
  (cond ((equal var val) subst)
        ((bound? var subst)
         (unify (lookup var subst) val subst))
        ;; New condition: dereference val when it is a variable
        ((and (var? val) (bound? val subst))
         (unify var (lookup val subst) subst))
        ((occurs-in? var val subst) 'fail)
        (t (extend-subst var val subst))))

(defun var? (x) "Is x a variable?" (member x '(X Y Z)))

(defun bound? (x subst) "Is x a bound variable?" (assoc x subst))

(defun lookup (var subst) (cdr (assoc var subst)))

(defun extend-subst (var val subst) (cons (cons var val) subst))

;;tests from PN paper
(aseq-test:deftest norvig-unify-tests-1
  (aseq-test:is (equal (unify '(p X Y) '(p Y X))
                    '((X . Y))))
  (aseq-test:is (equal (unify '(p X Y a) '(p Y X X))
                    '((Y . A) (X . Y)))))