#lang eopl

;; mapping
;; Proposito:
;; F x L1 x L2 -> L : Procedimiento que recibe una funcion unaria F y
;; dos listas de numeros L1 y L2 de igual tamaño, retorna una lista de
;; pares (a b) donde a pertenece a L1 y b pertenece a L2, cumpliendose
;; que F(a) = b.
;;
;; <lista> := ()
;;          := (<numero> <lista>)
;;
;; F  : funcion unaria
;; L1 : lista de numeros
;; L2 : lista de numeros

(define mapping
  (lambda (F L1 L2)
    (cond
      [(null? L1) '()] ;; primer caso, cuando la primera lista esta vacia, en ese caso retornamos una nueva lista vacia

      [else ;; de lo contrario empezamos evaluando el primer elemento de cada lista
       (if (= (F (car L1)) (car L2)) ;; a la funcion unaria, le paso el primer valor de la lista L1, y si ese valor se compara con el primero de la lista L2, si son iguales entonces entra
           (cons (list (car L1) (car L2)) (mapping F (cdr L1) (cdr L2))) ;; al ser iguales F(a) = b, entonces se construye la lista, como el par (a,b) y se sigue con los siguientes o el resto de los valores, comparandolos
           (mapping F (cdr L1) (cdr L2)) ;; de lo contrario, si la condicion no se cumple, o sea no son iguales, entonces continuo con el siguiente par, evaluando el resto de la lista
           )
       ]
      )
    )
  )

;; Pruebas
;; (mapping (lambda (d) (* d 2)) (list 1 2 3) (list 2 4 6))
;; (mapping (lambda (d) (* d 3)) (list 1 2 2) (list 2 4 6))
;; (mapping (lambda (d) (* d 2)) (list 1 2 3) (list 3 9 12))
