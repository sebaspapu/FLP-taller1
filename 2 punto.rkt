#lang eopl

;; down
;; Proposito:
;; L -> L' : Procedimiento que recibe una lista L y retorna una lista
;; con cada elemento de L envuelto en un nivel adicional de paréntesis.
;;
;; <lista> := ()
;;          := (<valor-de-scheme> <lista>)
;;
;; L : lista de valores

(define down ; defino la funcion dandole un nombre (etiqueta)
  (lambda (L) ; y ahora si creo la funcion lambda y creo un argumento
    (cond
      ; condiciones
      [(null? L) '()] ; primero evaluo si la lista está vacia, usando la funcion null, y retorno una lista vacia en caso de que lo este
      [else ; si no esta vacia entonces hago lo siguiente:
       ; creo una lista con cons
       ; a la lista le saco el primer valor, y creandolo como una lista usando list
       ; luego vuelvo y uso esta misma funcion down para hacer lo mismo con el resto de la lista
       (cons (list (car L)) (down (cdr L)))
       ]
      )
    )
  )

;; Pruebas
;; (down '(1 2 3))
;; (down '((una) (buena) (idea)))
;; (down '(un (objeto (mas)) complicado))