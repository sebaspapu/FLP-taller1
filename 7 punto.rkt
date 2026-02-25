#lang eopl

(define cartesian-product
  (lambda (L1 L2)
    (if (null? L1)
        '() ;si esta vacia la L1 entonces devuelvo una lista vacia
        (unir-listas (formar-pares (car L1) L2) (cartesian-product (cdr L1) L2)) ; antes de unir, primero formo los pares, usando la primera lista, y luego realizo lo mismo pero para el resto
        )
    )
  )

; funciones auxiliares

; construyo la funcion auxiliar para crear los pares

(define formar-pares
  (lambda (x L) ; recibo el primer valor de la lista L1 y la lista L2
    (if (null? L)
        '() ; devuelvo null cuando ya no hayan mas variables de la lista 2 para unir como un par
        (cons (list x (car L)) (formar-pares x (cdr L)) ; construyo entonces una lista con el primer termino de la Lista 1 y la lista 2, y luego repito ese proceso con el resto
              )
        )
    )
  )

; contruyo una funcion que me permite unir las listas

(define unir-listas
  (lambda (L1 L2) ; le paso ambas listas
    (if (null? L1) ; si la lista L1 esta vacia, entonces retorno solo la lista L2
        L2
        (cons (car L1) (unir-listas (cdr L1) L2)) ; construyo la lista con el primer valor de L1, y continuo con el resto
     )
   )
  )

; casos de prueba:
; (cartesian-product '(a b c) '(x y))
; (cartesian-product '(p q r) '(5 6 7))