#lang eopl

; todas estas retornan boleeanos, #t verdadero, #f falso
; number? , en este caso valida si es un numero
; symbol? , esta funcion valida si es un simbolo (una palabra)
; string? , valida si es una cadena de texto que va entre comillas
; null? , evalua si hay lista vacia
; car, para obtener el primer valor de la lista
; cdr, para obtener el resto de la lista

(define filter-in
  (lambda (P L)
    (cond
      [(null? L) '()] ; caso base, siempre evaluar si hay lista vacia, de ser asi retorna lista vacia
      [else
       ( if (P (car L)) ; Si el primer valor de la Lista, satisface la condicion del predicado
            (cons (car L) (filter-in P (cdr L))) ; si es verdadero, entonces crea una nueva lista, en donde dejamos ese valor, y continuamos evaluando los siguientes
            (filter-in P (cdr L)) ; si NO es verdadero, entonces continuo evaluando pero en el resto de la lista
            )]
      )
    )
  )

; (filter-in number? '(a 2 (1 3) b 7))
; (filter-in symbol? '(a (b c) 17 foo))
; (filter-in string? '(a b u "univalle" "racket" "flp" 28 90 (1 2 3)))