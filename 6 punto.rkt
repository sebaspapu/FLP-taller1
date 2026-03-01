#lang eopl

;; swapper
;; Proposito:
;; E1 x E2 x L -> L' : Procedimiento que recibe dos elementos E1 y E2
;; y una lista L, retorna una lista igual a L pero con cada ocurrencia
;; de E1 reemplazada por E2 y viceversa.
;;
;; <lista> := ()
;;          := (<valor-de-scheme> <lista>)
;;
;; E1 : valor de scheme (elemento a intercambiar)
;; E2 : valor de scheme (elemento a intercambiar)
;; L  : lista de valores

(define swapper
  (lambda (E1 E2 L)
    (cond
      [(null? L) '()] ;; comprobar si la lista esta vacia, si es asi retorno una lista vacia
      [else
       (cond
         [(eqv? (car L) E1) ;; evaluo si el primer elemento de la lista es igual a el primero de la lista
          (cons E2 (swapper E1 E2 (cdr L)))] ;; si es asi, entonces construyo una lista con el elemento 2, en esa posicion que tenia el E1, y sigo evaluando en el resto
         [(eqv? (car L) E2) ;; ahora si el primer elemento evaluado de la lista es igual a E2
          (cons E1 (swapper E1 E2 (cdr L)))] ;; entonces lo cambio y dejo el elemnto E1 en vez del E2, y sigo evaluando el resto de la lista
         [else
          (cons (car L) (swapper E1 E2 (cdr L)))] ;; si no es ninguno de los dos, entonces se deja igual ese primer elemento, y sigo evaluando el resto de la lista 
         )]
      )
    )
  )

;; Pruebas
;; (swapper 'a 'd '(a b c d))
;; (swapper 'a 'd '(a d () c d))
;; (swapper 'x 'y '(y y x y x y x x y))