#lang eopl

;; filter-acum
;; Proposito:
;; a x b x F x acum x filter -> valor : Procedimiento que aplica la
;; función binaria F a todos los elementos en el intervalo [a,b] que
;; cumplen con el predicado filter, acumulando el resultado en acum.
;;
;; a      : número (límite inferior del intervalo)
;; b      : número (límite superior del intervalo)
;; F      : función binaria
;; acum   : valor inicial del acumulador
;; filter : función unaria (predicado)

(define filter-acum
  (lambda (a b F acum filter)
    (if (> a b)
        acum
        (if (filter a)
            (filter-acum (+ a 1) b F (F acum a) filter)
            (filter-acum (+ a 1) b F acum filter)))))

;; Pruebas
;; (filter-acum 1 10 + 0 odd?)
;; (filter-acum 1 10 + 0 even?)