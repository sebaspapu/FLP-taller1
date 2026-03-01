#lang eopl

;; palindrome?
;; Proposito:
;; palabra -> boolean : Procedimiento que recibe una lista de simbolos
;; y retorna #t si es palindromo (se lee igual de izquierda a derecha
;; que de derecha a izquierda), #f en caso contrario.
;;
;; <lista> := ()
;;          := (<simbolo> <lista>)
;;
;; palabra : lista de simbolos

(define palindrome?
  (lambda (palabra)
    (son-iguales? palabra (invertir palabra)) ;; se comprueba que sean iguales
    )
  )

;; invertir
;; Proposito:
;; L -> L' : Procedimiento auxiliar que retorna la lista L invertida.
;;
;; L : lista de valores

(define invertir
  (lambda (L)
    (if (null? L) ;; verifico si la lista esta vacia
        '() ;; si es verdadero, devuelvo la lista vacia
        (insertar-al-final (car L) (invertir (cdr L)))) ;; uso la funcion auxiliar y le paso el primer valor, y el resto de la lista
    ;; aqui invertir se ejecuta hasta que obtengamos una la lista vacia:
    ))

;; insertar-al-final
;; Proposito:
;; x x L -> L' : Procedimiento auxiliar que inserta el elemento x
;; al final de la lista L.
;;
;; x : valor de scheme
;; L : lista de valores

(define insertar-al-final
  (lambda (x L)
    (if (null? L) ;; compruebo si la lista esta vacia, la que viene de invertir
        (list x) ;; en caso de estar vacia, entonces retornamos el valor que quedo como ultimo, y esto dentro de una lista
        (cons (car L) (insertar-al-final x (cdr L))) ;; se construye la lista con el primer valor de la Lista, dejandolo por fuera, y repitiendo el mismo proceso hasta conseguir dejarla al reves
        ))
  )

;; son-iguales?
;; Proposito:
;; L1 x L2 -> boolean : Procedimiento auxiliar que compara dos listas
;; elemento a elemento, retorna #t si son iguales, #f en caso contrario.
;;
;; L1 : lista de valores
;; L2 : lista de valores

(define son-iguales?
  (lambda (L1 L2) ;; evalua ambas listas
    (cond
      [(and (null? L1) (null? L2)) #t] ;; comprueba si ambas listas estan vacias , de ser asi devuelve #t
      [(or (null? L1) (null? L2)) #f] ;; si alguna de las dos esta vacia, quiere decir que ya hay una que no se va a parecer a la otra, por tanto no son iguales
      [(eqv? (car L1) (car L2)) (son-iguales? (cdr L1) (cdr L2))] ;; compara los primeros valores de la lista, si son iguales, entonces sigue evaluando el resto
      [else #f] ;; de lo contrario no se parecen o no son iguales, por tanto devuelve falso #f
      )
    )
  )

;; Pruebas:
;; (palindrome? '(r a d a r))
;; (palindrome? '(n e u q u e n))
;; (palindrome? '(h o l a))