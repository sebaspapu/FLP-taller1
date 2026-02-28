#lang eopl
;exercise 3

; <movimiento> := (simbolo simbolo)
; <lista-movimientos> := ()
;                      := (cons movimiento lista-movimientos)
(define hanoi
  (lambda (n origen auxiliar destino)
    (cond
      [(= n 0)
       '()]
      [(= n 1)
       (list (list origen destino))] ;se mueve el disco grande
      [else
       (append
        (hanoi (- n 1) origen destino auxiliar) ;trucamos aux y destino para llevar los discos pequeños  al aux
        (list (list origen destino))
        (hanoi (- n 1) auxiliar origen destino)) ;discos pequños a destino
       ])))

;Casos de prueba:
;(hanoi 0 'A 'B 'C)
;(hanoi 3 'C 'A 'B)
;(hanoi 4 'x 'y 'z)