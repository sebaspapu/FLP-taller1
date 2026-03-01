#lang eopl

;; invert
;; Proposito:
;; L x P -> L' : Procedimiento que recibe una lista L de pares (x y)
;; y un predicado P, retorna una lista con los pares invertidos (y x)
;; solo cuando ambos elementos cumplen con el predicado P.
;;
;; <lista> := ()
;;          := (<par> <lista>)
;; <par>   := (<valor-de-scheme> <valor-de-scheme>)
;;
;; L : lista de pares
;; P : predicado unario


(define invert
  (lambda (L P)
    (cond ;; defino el condicional para verificar varias condiciones
      [(null? L) '()] ;; caso base es preguntar si la lista esta vacia o no, en caso de que si, pues la retorna vacia
      [else ;; ahora cuando si hay una lista llena

       (let(
            ;; zona de declaraciones del let
            [par (car L)];; declaro la lista y la guardo en una variable llamada par
            )
         ;; ahora voy a comprobar si tanto el primer valor de la lista car, como el segundo son pares
        (let (
              ;; zona de declaraciones del let
              [x (car par)] ;; accedo al primer valor de la lista par
              [y (cadr par)] ;; accedo al segundo valor de la lista par
              )
          ;; ahora vamos a comprobar si tanto x como "y" son pares, usando el predicado
          (if (and (P x) (P y)) ;; si x y "y" son pares entra aqui
              (cons (list y x) (invert (cdr L) P)) ;; se construye una lista, en donde el primer valor es la lista invertida, y como tal la lista (obligatoria) es la comprobacion de pares en el resto de la lista L
              (invert (cdr L) P) ;; de lo contrario solo sigue comprobando la lista L en busca de pares
           )
          )
        )
       ]
      )
   )
)

;; Funcion auxiliar
(define multiplo5?
  (lambda (n)
    (= (modulo n 5) 0)))

;; Pruebas
;; (invert '((3 2) (2 4)) even?)
;; (invert '() even?)