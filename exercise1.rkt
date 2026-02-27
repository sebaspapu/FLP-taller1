#lang eopl
;exercise 1


;  < ́arbol-binario> := ( ́arbol-vacıo) empty
;  := (nodo) n ́umero < ́arbol-binario> < ́arbol-binario>


; left (1 () ()) cdr car
; right cddar
(define my-append(lambda (l1 l2)
   (if(null? l1) l2 (cons (car l1) (my-append (cdr l1)l2)))
 ))

; <lista> := empty
; <lista> := int <lista>
(define invert(lambda(l1)
    (if(null? l1) empty
         (my-append (invert (cdr l1)) (list (car l1)))
       )
 ))

(invert '(1 2 3 4 5))

(define helper (lambda (n bst [lst '()])
    (cond
        [(null? bst) empty]
        [(equal? n (car bst)) lst]
        [(> (car bst) n) (helper n (cadr bst) (cons 'left lst))]
        [(< (car bst) n) (helper n (caddr bst) (cons 'right lst))]
        [else empty]

      )
 ))

(define path (lambda (n bst [lst '()])(
    invert (helper n bst lst)
 )))
