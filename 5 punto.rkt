#lang eopl

; cat: para mirar el valor actual
; cdr: para obtener el resto de la lista
; eqv?: compara simbolos, numeros y booleanos, si son iguales retorna #t, de lo contrario #f

; esta es mi funcion principal
(define palindrome?
  (lambda (palabra)
    (son-iguales? palabra (invertir palabra)) ; se comprueba que sean iguales
    )
  )

; ahora las funciones auxiliares:

; voltear la lista de forma recursiva
(define invertir
  (lambda (L)
    (if (null? L) ; verifico si la lista esta vacia
        '() ; si es verdadero, devuelvo la lista vacia
        (insertar-al-final (car L) (invertir (cdr L)))) ; uso la funcion auxiliar y le paso el primer valor, y el resto de la lista
    ; aqui invertir se ejecuta hasta que obtengamos una la lista vacia:
    ))

; luego aca, de forma recursiva, inserto al final cada valor, de modo cada vez que ingresa a la funcion la palabra va quedando invertida
(define insertar-al-final
  (lambda (x L)
    (if (null? L) ; compruebo si la lista esta vacia, la que viene de invertir
        (list x) ; en caso de estar vacia, entonces retornamos el valor que quedo como ultimo, y esto dentro de una lista
        (cons (car L) (insertar-al-final x (cdr L))) ; se construye la lista con el primer valor de la Lista, dejandolo por fuera, y repitiendo el mismo proceso hasta conseguir dejarla al reves
        ))
  )

; ahora para comprobar si la palabra actual y la invertidad son iguales
(define son-iguales?
  (lambda (L1 L2) ; evalua ambas listas
    (cond
      [(and (null? L1) (null? L2)) #t] ; comprueba si ambas listas estan vacias , de ser asi devuelve #t
      [(or (null? L1) (null? L2)) #f] ; si alguna de las dos esta vacia, quiere decir que ya hay una que no se va a parecer a la otra, por tanto no son iguales
      [(eqv? (car L1) (car L2)) (son-iguales? (cdr L1) (cdr L2))] ; compara los primeros valores de la lista, si son iguales, entonces sigue evaluando el resto
      [else #f] ; de lo contrario no se parecen o no son iguales, por tanto devuelve falso #f
      )
    )
  )

; casos de prueba:
; (palindrome? '(r a d a r))
; (palindrome? '(n e u q u e n))
; (palindrome? '(h o l a))