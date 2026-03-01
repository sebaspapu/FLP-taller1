#lang eopl

;; inversions
;; Proposito:
;; L -> int : Procedimiento que recibe una lista de numeros diferentes L
;; y retorna el numero total de inversiones. Una inversion es un par (i j)
;; donde i < j (posicion) pero ai > aj (valor).
;;
;; <lista> := ()
;;          := (<numero> <lista>)
;;
;; L : lista de numeros diferentes

(define inversions
  (lambda (L)
    (if (null? L)
        0 ;; Caso base: una lista vacía tiene 0 inversiones.
        ;; Aqui,vamos sumando las inversiones del primer elemento contra el resto, más las inversiones que existan en el resto de la lista.
        (+ (contar-menores (car L) (cdr L))(inversions (cdr L))))
    )
  )

;; contar-menores
;; Proposito:
;; x x L -> int : Procedimiento auxiliar que cuenta cuantos elementos
;; de la lista L son menores que x.
;;
;; x : numero
;; L : lista de numeros

(define contar-menores
  (lambda (x L)
    (cond
      [(null? L) 0]
      ;; Aqui, si el elemento actual es menor que x, contamos 1 y seguimos.
      [(< (car L) x) (+ 1 (contar-menores x (cdr L)))]
      ;; Si no, pues seguimos sin sumar nada.
      [else (contar-menores x (cdr L))])))

;; Pruebas:
;; (inversions '(2 3 8 6 1))
;; (inversions '(1 2 3 4))
;; (inversions '(3 2 1))