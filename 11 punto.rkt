#lang eopl

;; zip 
;; Proposito:
;; F x L1 x L2 -> L : Procedimiento que recibe una función binaria F
;; y dos listas L1 y L2 de igual tamaño, retorna una lista donde la
;; posición n-ésima corresponde al resultado de aplicar F sobre los
;; elementos en la posición n-ésima de L1 y L2.
;;
;; <lista> := ()
;;          := (<valor-de-scheme> <lista>)
;;
;; F: función binaria
;; L1: lista de valores
;; L2: lista de valores (mismo tamaño que L1)

(define zip
  (lambda (F L1 L2)
    (if (null? L1)
        '()
        (cons (F (car L1) (car L2))
              (zip F (cdr L1) (cdr L2))))))

;; Pruebas
;; (zip + '(1 4) '(6 2))
;; (zip * '(11 5 6) '(10 9 8))