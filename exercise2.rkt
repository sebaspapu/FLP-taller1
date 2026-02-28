#lang eopl
;exercise 2

; pairs := empty
; pairs := (<int>, <int>)
(define pairs_sum (lambda (l1 l2)
    (list (+(car l1) (car l2)) (+(cadr l1) (cadr l2)))
 ))

; sum-left1 : (list int int) -> (list int int)
; incrementa en 1 la cantidad de números pares
(define sum-left1 (lambda (lst)(
   cons (+ (car lst) 1) (cdr lst)
 )))

; sum-right1 : (list int int) -> (list int int)
; incrementa en 1 la cantidad de números impares
(define sum-right1 (lambda (lst)(
   list (car lst) (+(cadr lst)1)
 )))


; count-odd-and-even : bst -> (list int int)
; recorre recursivamente el árbol binario y retorna una lista con:
;   - la cantidad de números pares
;   - la cantidad de números impares
(define count-odd-and-even (lambda (bst)(
    cond
     [(null? bst) '(0 0)]
     [(odd? (car bst)) (sum-right1
                        (pairs_sum (count-odd-and-even (cadr bst))
                                   (count-odd-and-even (caddr bst))))]
     [(even? (car bst)) (sum-left1
                         (pairs_sum (count-odd-and-even (cadr bst))
                                    (count-odd-and-even (caddr bst))))]
     [else '(0 0)]
 )))

;Casos de prueba
;(count-odd-and-even '())
;(count-odd-and-even '(8(4 (2 () ()) (6 () ()))(10 () (12 () ()))))
;(count-odd-and-even '(5 (3 (1 () ()) ()) (8 () (10 (9 () ()) ()))))