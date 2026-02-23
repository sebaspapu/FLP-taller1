#lang eopl

; zero?: esta funcion pregunta si un numero es exactamente igual a cero
; si es asi entonces devuelve un #t verdadero, y #f si es falso
; es el equivalente a escribir (= n 0)
; cdr: para obtener el resto de la lista
; por convencion, cuando una funcion tiene al final un ?, significa que devuelve un valor booleano

(define list-set
  (lambda (L n x P)
    (cond
      [(null? L) '()] ; caso base, si la lista L esta vacia, retorna vacio
      [(zero? n) ; evaluo si n es igual a 0
       (if (P (car L)) ; ahora evaluo el primer valor de la Lista, usando el predicado
           (cons x (cdr L)) ; Si se cumple, entonces creo una nueva lista, poniendo lo de x como primer valor, y pongo el resto de lo que tenia L
           (cons (car L) (cdr L)) ; si No se cumple, entonces lo dejo igual, y pongo el resto de la lista
       )] 
      [else ; cuando la posicion n no es cero
       (cons (car L) (list-set (cdr L) (- n 1) x P)) ; entonces dejo el primer valor aca, y sigo evaluando apartir del resto, disminuyendo la cantidad n
       ]
      )
    )
  )

; funcion auxiliar para el caso de prueba mayor a 5:
(define mayor5?
 (lambda (valor)
  (> valor 5)
  )
 )

; casos de prueba:
;(list-set '(5 8 7 6) 2 '(1 2) odd?)
;(list-set '(5 8 7 6) 2 '(1 2) even?)
;(list-set '(5 8 7 6) 3 '(1 5 10) mayor5?)
;(list-set '(5 8 7 6) 0 '(1 5 10) mayor5? )