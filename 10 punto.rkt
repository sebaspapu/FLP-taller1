#lang eopl

;; balanced-parentheses?
;; Proposito:
;; L -> boolean : Procedimiento que recibe una lista de simbolos L
;; cuyos elementos representan parentesis (O para apertura, C para
;; cierre), retorna #t si estan correctamente balanceados, #f en
;; caso contrario.
;;
;; <lista> := ()
;;          := (<simbolo> <lista>)
;;
;; L : lista de simbolos (O o C)

(define balanced-parentheses?
  (lambda (L)
    ;; Iniciamos el contador en 0
    (verificar-balance L 0)))

;; verificar-balance
;; Proposito:
;; L x contador -> boolean : Procedimiento auxiliar que recorre la
;; lista acumulando en contador las aperturas y cierres. Retorna #t
;; si al final el contador es 0 y nunca fue negativo.
;;
;; L        : lista de simbolos
;; contador : numero entero (aperturas pendientes de cerrar)

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