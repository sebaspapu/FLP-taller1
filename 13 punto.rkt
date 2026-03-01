#lang eopl

;; operate
;; Proposito:
;; lrators x lrands -> valor : Procedimiento que recibe una lista de
;; funciones binarias lrators de tamaño n y una lista de números lrands
;; de tamaño n+1, retorna el resultado de aplicar sucesivamente las
;; operaciones en lrators a los valores en lrands.
;;
;; <lista-operadores> := ()
;;                     := (<función-binaria> <lista-operadores>)
;;
;; <lista-operandos>  := (<número>)
;;                     := (<número> <lista-operandos>)
;;
;; lrators : lista de funciones binarias (tamaño n)
;; lrands  : lista de números (tamaño n+1)

(define operate
  (lambda (lrators lrands)
    (if (null? lrators)
        (car lrands)
        (operate (cdr lrators)
                 (cons ((car lrators) (car lrands) (car (cdr lrands)))
                       (cdr (cdr lrands)))))))

;; Pruebas
;; (operate (list + * + - *) '(1 2 8 4 11 6))
;; (operate (list *) '(4 5))