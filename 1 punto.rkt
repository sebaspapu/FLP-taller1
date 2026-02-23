#lang eopl


; car para obtener el primer valor de la lista
; cdr para obtener el resto de la lista
; cadr para obtener el primero del resto


(define invert ; defino la funcion
  (lambda (L P) ; defino los parametros que va a recibir, en este caso la lista L y un predicado P
    (cond ; defino el condicional para verificar varias condiciones
      [(null? L) '()] ; caso base es preguntar si la lista esta vacia o no, en caso de que si, pues la retorna vacia
      [else ; ahora cuando si hay una lista llena

       (let(
            ; zona de declaraciones del let
            [par (car L)]; declaro la lista y la guardo en una variable llamada par
            )
         ; ahora voy a comprobar si tanto el primer valor de la lista car, como el segundo son pares
        (let (
              ; zona de declaraciones del let
              [x (car par)] ; accedo al primer valor de la lista par
              [y (cadr par)] ; accedo al segundo valor de la lista par
              )
          ; ahora vamos a comprobar si tanto x como "y" son pares, usando el predicado
          (if (and (P x) (P y)) ; si x y "y" son pares entra aqui
              (cons (list y x) (invert (cdr L) P)) ; se construye una lista, en donde el primer valor es la lista invertida, y como tal la lista (obligatoria) es la comprobacion de pares en el resto de la lista L
              (invert (cdr L) P) ; de lo contrario solo sigue comprobando la lista L en busca de pares
           )
          )
        )
       ]
      )
   )
)

; esto vendria siendo la funcion auxiliar para los casos de prueba
(define multiplo5?
  (lambda (n)
    (= (modulo n 5) 0)))

; (invert '((3 2) (2 4)) even?) --> lista con dos sublistas y un predicado que prueba si son pares
; (invert '() even?) --> lista vacia