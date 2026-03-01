#lang eopl

;; Nombres y Codigo:
;; Sebastian Bolaños Morales - (2310168)
;; Jairo Hernan Gonzalez Barreto - (2324314)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; down
;; Proposito:
;; L -> L' : Procedimiento que recibe una lista L y retorna una lista
;; con cada elemento de L envuelto en un nivel adicional de paréntesis.
;;
;; <lista> := ()
;;          := (<valor-de-scheme> <lista>)
;;
;; L : lista de valores

(define down ; defino la funcion dandole un nombre (etiqueta)
  (lambda (L) ; y ahora si creo la funcion lambda y creo un argumento
    (cond
      ; condiciones
      [(null? L) '()] ; primero evaluo si la lista está vacia, usando la funcion null, y retorno una lista vacia en caso de que lo este
      [else ; si no esta vacia entonces hago lo siguiente:
       ; creo una lista con cons
       ; a la lista le saco el primer valor, y creandolo como una lista usando list
       ; luego vuelvo y uso esta misma funcion down para hacer lo mismo con el resto de la lista
       (cons (list (car L)) (down (cdr L)))
       ]
      )
    )
  )

;; Pruebas
;; (down '(1 2 3))
;; (down '((una) (buena) (idea)))
;; (down '(un (objeto (mas)) complicado))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; list-set
;; Proposito:
;; L x n x x x P -> L' : Procedimiento que recibe una lista L, una
;; posicion n, un elemento x y un predicado P. Retorna una lista igual
;; a L pero con el elemento en la posicion n reemplazado por x, solo
;; si el elemento original cumple con P.
;;
;; <lista> := ()
;;          := (<valor-de-scheme> <lista>)
;;
;; L : lista de valores
;; n : numero (posicion, indexando desde cero)
;; x : valor de scheme (elemento de reemplazo)
;; P : predicado unario

(define list-set
  (lambda (L n x P)
    (cond
      [(null? L) '()] ;; caso base, si la lista L esta vacia, retorna vacio
      [(zero? n) ;; evaluo si n es igual a 0
       (if (P (car L)) ;; ahora evaluo el primer valor de la Lista, usando el predicado
           (cons x (cdr L)) ;; Si se cumple, entonces creo una nueva lista, poniendo lo de x como primer valor, y pongo el resto de lo que tenia L
           (cons (car L) (cdr L)) ;; si No se cumple, entonces lo dejo igual, y pongo el resto de la lista
       )] 
      [else ;; cuando la posicion n no es cero
       (cons (car L) (list-set (cdr L) (- n 1) x P)) ;; entonces dejo el primer valor aca, y sigo evaluando apartir del resto, disminuyendo la cantidad n
       ]
      )
    )
  )

;; Funcion auxiliar
(define mayor5?
 (lambda (valor)
  (> valor 5)
  )
 )

;; Pruebas
;; (list-set '(5 8 7 6) 2 '(1 2) odd?)
;; (list-set '(5 8 7 6) 2 '(1 2) even?)
;; (list-set '(5 8 7 6) 3 '(1 5 10) mayor5?)
;; (list-set '(5 8 7 6) 0 '(1 5 10) mayor5? )


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; filter-in
;; Proposito:
;; P x L -> L' : Procedimiento que recibe un predicado P y una lista L,
;; retorna una lista con los elementos de L que satisfacen P.
;;
;; <lista> := ()
;;          := (<valor-de-scheme> <lista>)
;;
;; P : predicado unario
;; L : lista de valores

(define filter-in
  (lambda (P L)
    (cond
      [(null? L) '()] ;; caso base, siempre evaluar si hay lista vacia, de ser asi retorna lista vacia
      [else
       ( if (P (car L)) ;; Si el primer valor de la Lista, satisface la condicion del predicado
            (cons (car L) (filter-in P (cdr L))) ;; si es verdadero, entonces crea una nueva lista, en donde dejamos ese valor, y continuamos evaluando los siguientes
            (filter-in P (cdr L)) ;; si NO es verdadero, entonces continuo evaluando pero en el resto de la lista
            )]
      )
    )
  )

;; Pruebas
;; (filter-in number? '(a 2 (1 3) b 7))
;; (filter-in symbol? '(a (b c) 17 foo))
;; (filter-in string? '(a b u "univalle" "racket" "flp" 28 90 (1 2 3)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; palindrome?
;; Proposito:
;; palabra -> boolean : Procedimiento que recibe una lista de simbolos
;; y retorna #t si es palindromo (se lee igual de izquierda a derecha
;; que de derecha a izquierda), #f en caso contrario.
;;
;; <lista> := ()
;;          := (<simbolo> <lista>)
;;
;; palabra : lista de simbolos

(define palindrome?
  (lambda (palabra)
    (son-iguales? palabra (invertir palabra)) ;; se comprueba que sean iguales
    )
  )

;; invertir
;; Proposito:
;; L -> L' : Procedimiento auxiliar que retorna la lista L invertida.
;;
;; L : lista de valores

(define invertir
  (lambda (L)
    (if (null? L) ;; verifico si la lista esta vacia
        '() ;; si es verdadero, devuelvo la lista vacia
        (insertar-al-final (car L) (invertir (cdr L)))) ;; uso la funcion auxiliar y le paso el primer valor, y el resto de la lista
    ;; aqui invertir se ejecuta hasta que obtengamos una la lista vacia:
    ))

;; insertar-al-final
;; Proposito:
;; x x L -> L' : Procedimiento auxiliar que inserta el elemento x
;; al final de la lista L.
;;
;; x : valor de scheme
;; L : lista de valores

(define insertar-al-final
  (lambda (x L)
    (if (null? L) ;; compruebo si la lista esta vacia, la que viene de invertir
        (list x) ;; en caso de estar vacia, entonces retornamos el valor que quedo como ultimo, y esto dentro de una lista
        (cons (car L) (insertar-al-final x (cdr L))) ;; se construye la lista con el primer valor de la Lista, dejandolo por fuera, y repitiendo el mismo proceso hasta conseguir dejarla al reves
        ))
  )

;; son-iguales?
;; Proposito:
;; L1 x L2 -> boolean : Procedimiento auxiliar que compara dos listas
;; elemento a elemento, retorna #t si son iguales, #f en caso contrario.
;;
;; L1 : lista de valores
;; L2 : lista de valores

(define son-iguales?
  (lambda (L1 L2) ;; evalua ambas listas
    (cond
      [(and (null? L1) (null? L2)) #t] ;; comprueba si ambas listas estan vacias , de ser asi devuelve #t
      [(or (null? L1) (null? L2)) #f] ;; si alguna de las dos esta vacia, quiere decir que ya hay una que no se va a parecer a la otra, por tanto no son iguales
      [(eqv? (car L1) (car L2)) (son-iguales? (cdr L1) (cdr L2))] ;; compara los primeros valores de la lista, si son iguales, entonces sigue evaluando el resto
      [else #f] ;; de lo contrario no se parecen o no son iguales, por tanto devuelve falso #f
      )
    )
  )

;; Pruebas:
;; (palindrome? '(r a d a r))
;; (palindrome? '(n e u q u e n))
;; (palindrome? '(h o l a))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; swapper
;; Proposito:
;; E1 x E2 x L -> L' : Procedimiento que recibe dos elementos E1 y E2
;; y una lista L, retorna una lista igual a L pero con cada ocurrencia
;; de E1 reemplazada por E2 y viceversa.
;;
;; <lista> := ()
;;          := (<valor-de-scheme> <lista>)
;;
;; E1 : valor de scheme (elemento a intercambiar)
;; E2 : valor de scheme (elemento a intercambiar)
;; L  : lista de valores

(define swapper
  (lambda (E1 E2 L)
    (cond
      [(null? L) '()] ;; comprobar si la lista esta vacia, si es asi retorno una lista vacia
      [else
       (cond
         [(eqv? (car L) E1) ;; evaluo si el primer elemento de la lista es igual a el primero de la lista
          (cons E2 (swapper E1 E2 (cdr L)))] ;; si es asi, entonces construyo una lista con el elemento 2, en esa posicion que tenia el E1, y sigo evaluando en el resto
         [(eqv? (car L) E2) ;; ahora si el primer elemento evaluado de la lista es igual a E2
          (cons E1 (swapper E1 E2 (cdr L)))] ;; entonces lo cambio y dejo el elemnto E1 en vez del E2, y sigo evaluando el resto de la lista
         [else
          (cons (car L) (swapper E1 E2 (cdr L)))] ;; si no es ninguno de los dos, entonces se deja igual ese primer elemento, y sigo evaluando el resto de la lista 
         )]
      )
    )
  )

;; Pruebas
;; (swapper 'a 'd '(a b c d))
;; (swapper 'a 'd '(a d () c d))
;; (swapper 'x 'y '(y y x y x y x x y))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; cartesian-product
;; Proposito:
;; L1 x L2 -> L : Procedimiento que recibe dos listas de simbolos L1 y L2
;; sin repeticiones, retorna una lista de tuplas que representan el
;; producto cartesiano entre L1 y L2.
;;
;; <lista> := ()
;;          := (<simbolo> <lista>)
;;
;; L1 : lista de simbolos
;; L2 : lista de simbolos

(define cartesian-product
  (lambda (L1 L2)
    (if (null? L1)
        '() ;; si esta vacia la L1 entonces devuelvo una lista vacia
        (unir-listas (formar-pares (car L1) L2) (cartesian-product (cdr L1) L2)) ;; antes de unir, primero formo los pares, usando la primera lista, y luego realizo lo mismo pero para el resto
        )
    )
  )

;; formar-pares
;; Proposito:
;; x x L -> L' : Procedimiento auxiliar que forma todos los pares
;; posibles entre el elemento x y cada elemento de la lista L.
;;
;; x : simbolo
;; L : lista de simbolos

(define formar-pares
  (lambda (x L) ;; recibo el primer valor de la lista L1 y la lista L2
    (if (null? L)
        '() ;; devuelvo null cuando ya no hayan mas variables de la lista 2 para unir como un par
        (cons (list x (car L)) (formar-pares x (cdr L)) ;; construyo entonces una lista con el primer termino de la Lista 1 y la lista 2, y luego repito ese proceso con el resto
              )
        )
    )
  )

;; unir-listas
;; Proposito:
;; L1 x L2 -> L : Procedimiento auxiliar que concatena dos listas.
;;
;; L1 : lista de valores
;; L2 : lista de valores

(define unir-listas
  (lambda (L1 L2) ;; le paso ambas listas
    (if (null? L1) ;; si la lista L1 esta vacia, entonces retorno solo la lista L2
        L2
        (cons (car L1) (unir-listas (cdr L1) L2)) ;; construyo la lista con el primer valor de L1, y continuo con el resto
     )
   )
  )

;; Pruebas
;; (cartesian-product '(a b c) '(x y))
;; (cartesian-product '(p q r) '(5 6 7))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; mapping
;; Proposito:
;; F x L1 x L2 -> L : Procedimiento que recibe una funcion unaria F y
;; dos listas de numeros L1 y L2 de igual tamaño, retorna una lista de
;; pares (a b) donde a pertenece a L1 y b pertenece a L2, cumpliendose
;; que F(a) = b.
;;
;; <lista> := ()
;;          := (<numero> <lista>)
;;
;; F  : funcion unaria
;; L1 : lista de numeros
;; L2 : lista de numeros

(define mapping
  (lambda (F L1 L2)
    (cond
      [(null? L1) '()] ;; primer caso, cuando la primera lista esta vacia, en ese caso retornamos una nueva lista vacia

      [else ;; de lo contrario empezamos evaluando el primer elemento de cada lista
       (if (= (F (car L1)) (car L2)) ;; a la funcion unaria, le paso el primer valor de la lista L1, y si ese valor se compara con el primero de la lista L2, si son iguales entonces entra
           (cons (list (car L1) (car L2)) (mapping F (cdr L1) (cdr L2))) ;; al ser iguales F(a) = b, entonces se construye la lista, como el par (a,b) y se sigue con los siguientes o el resto de los valores, comparandolos
           (mapping F (cdr L1) (cdr L2)) ;; de lo contrario, si la condicion no se cumple, o sea no son iguales, entonces continuo con el siguiente par, evaluando el resto de la lista
           )
       ]
      )
    )
  )

;; Pruebas
;; (mapping (lambda (d) (* d 2)) (list 1 2 3) (list 2 4 6))
;; (mapping (lambda (d) (* d 3)) (list 1 2 2) (list 2 4 6))
;; (mapping (lambda (d) (* d 2)) (list 1 2 3) (list 3 9 12))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;











