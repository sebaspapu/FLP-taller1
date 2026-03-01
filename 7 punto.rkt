#lang eopl

;; cartesian-product
;; Proposito:
;; L1 x L2 -> L : Procedimiento que recibe dos listas de simbolos L1 y L2
;; sin repeticiones, retorna una lista de tuplas que representan el
;; producto cartesiano entre L1 y L2.
;;
;; <lista> := ()
;;          := (<simbolo> <lista>)
;;
;; L1 : lista de simbolos
;; L2 : lista de simbolos

(define cartesian-product
  (lambda (L1 L2)
    (if (null? L1)
        '() ;; si esta vacia la L1 entonces devuelvo una lista vacia
        (unir-listas (formar-pares (car L1) L2) (cartesian-product (cdr L1) L2)) ;; antes de unir, primero formo los pares, usando la primera lista, y luego realizo lo mismo pero para el resto
        )
    )
  )

;; formar-pares
;; Proposito:
;; x x L -> L' : Procedimiento auxiliar que forma todos los pares
;; posibles entre el elemento x y cada elemento de la lista L.
;;
;; x : simbolo
;; L : lista de simbolos

(define formar-pares
  (lambda (x L) ;; recibo el primer valor de la lista L1 y la lista L2
    (if (null? L)
        '() ;; devuelvo null cuando ya no hayan mas variables de la lista 2 para unir como un par
        (cons (list x (car L)) (formar-pares x (cdr L)) ;; construyo entonces una lista con el primer termino de la Lista 1 y la lista 2, y luego repito ese proceso con el resto
              )
        )
    )
  )

;; unir-listas
;; Proposito:
;; L1 x L2 -> L : Procedimiento auxiliar que concatena dos listas.
;;
;; L1 : lista de valores
;; L2 : lista de valores

(define unir-listas
  (lambda (L1 L2) ;; le paso ambas listas
    (if (null? L1) ;; si la lista L1 esta vacia, entonces retorno solo la lista L2
        L2
        (cons (car L1) (unir-listas (cdr L1) L2)) ;; construyo la lista con el primer valor de L1, y continuo con el resto
     )
   )
  )

;; Pruebas
;; (cartesian-product '(a b c) '(x y))
;; (cartesian-product '(p q r) '(5 6 7))