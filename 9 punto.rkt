#lang eopl

;; inversions
;; Propósito:
;; L -> int : Se recibe una lista de números diferentes L y retorna 
;; el número total de inversiones encontradas.

(define inversions
  (lambda (L)
    (if (null? L)
        0 ;; Caso base: una lista vacía tiene 0 inversiones.
        ;; Aqui,vamos sumando las inversiones del primer elemento contra el resto, más las inversiones que existan en el resto de la lista.
        (+ (contar-menores (car L) (cdr L))(inversions (cdr L))))
    )
  )

; funciones auxiliares:

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