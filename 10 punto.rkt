#lang eopl

;; balanced-parentheses?
;; Propósito:
;; L -> boolean : Recibe una lista de símbolos 'O y 'C y retorna #t si
;; está balanceada según las reglas, de lo contrario #f.

(define balanced-parentheses?
  (lambda (L)
    ;; Iniciamos el contador en 0
    (verificar-balance L 0)))

;; funcion auxiliar: verificar-balance : 
;; Propósito: Funcion auxiliar que recorre la lista manteniendo el conteo de aperturas.
(define verificar-balance
  (lambda (L contador)
    (cond
      ;; Si el contador es negativo en cualquier punto, hay más cierres que aperturas
      [(< contador 0) #f]
      
      ;; Caso base: si llegamos al final, el contador debe ser exactamente 0
      [(null? L) (= contador 0)]
      
      ;; Caso inductivo: En donde procesamos el primer elemento
      [else
       (cond
         ;; Si encontramos 'O (Apertura),, sumamos 1 al contador
         [(eqv? (car L) 'O) (verificar-balance (cdr L) (+ contador 1))]
         ;; Si encontramos 'C (Cierre), restamos 1 al contador
         [(eqv? (car L) 'C) (verificar-balance (cdr L) (- contador 1))]
         ;; Si hubiera otros símbolos, los ignoramos
         [else (verificar-balance (cdr L) contador)])])))

;; Pruebas
;; (balanced-parentheses? '(O C C O O C))
;; (balanced-parentheses? '(O C C))
;; (balanced-parentheses? '(O O C C))