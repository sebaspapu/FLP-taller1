# FLP-taller1

# Taller 1: Definicion recursiva de programas e induccion (Racket)

### Evaluacion por Indicadores - RES 047

Los indicadores de logro a evaluar en este Taller son:

- IL 1.1.1 Construye datos recursivamente utilizando metodos de induccion y gramaticas BNF (ejercicios 1 a 13)
- IL 1.1.2 Utiliza gramaticas BNF para guiar la construccion de programas recursivos (ejercicios 14 a 15)

Este taller tiene un valor global de 5.44% distribuidos en (3.2%) para el IL 1.1.1 y (2.24%) para el IL 1.1.2. El taller se calificara con una nota en
escala de 0 a 100 puntos (que luego se escalar ́a de 0.0 a 5.0). El valor de cada indicador se muestra a continuacion:

```
= 59puntos(aprox)
= 41puntos(aprox)
```
Cada ejercicio ser ́a probado con un conjunto de pruebas unitarias que deberá pasar para asignar la totalidad de los puntos indicados. En caso de
no pasar las pruebas, el valor a asignar ser ́a de 0.0pts. Asegúrese de respetar el contrato de cada función.


### Ejercicios - IL 1.1.1 (59pts)

1. (4.5pts)Elabore una funci ́on llamadainvertque recibe como argumentos:
una listaLy un predicadoP. La listaLse compone de paresx, yque a su
vez son listas (de tama ̃no 2). La funci ́on debe retornar una lista similar aL,
con pares ordenados invertidos, es decir,y, x, s ́olo cuando ambos elementos
de la lista cumplan con un predicadoP.Ejemplos:

> (invert ’((3 2) (4 2) (1 5) (2 8)) even?)

((2 4) (8 2))

> (invert ’((5 9) (10 90) (82 7) ) multiplo5? )

((90 10))
> (invert ’((6 9) (10 90) (82 7) ) odd? )

()

2. (4.5pts)Elabore una funci ́on llamadadownque recibe como argumento
una listaL, y lo que debe realizar dicha funci ́on es retornar una lista con
cada elemento deLasociado a un nivel m ́as de par ́entesis comparado con su
estado original enL.Ejemplos:

> (down ’(1 2 3))

((1) (2) (3))

> (down ’((una) (buena) (idea)))

(((una)) ((buena)) ((idea)))

> (down ’(un (objeto (mas)) complicado))

((un) ((objeto (mas))) (complicado))

3. (4.5pts)Elabore una funci ́on llamadalist-setque reciba cuatro argumen-
tos: una listaL, un n ́umeron, un elementoxy un predicadoP. La funci ́on
debe retornar una lista similar a la que recibe (L), pero debe tener en la
posici ́on ingresadan(indexando desde cero) el elementoxs ́olo si el elemento
original de la lista cumple con el predicadoP.Ejemplos:

> (list-set ’(5 8 7 6) 2 ’(1 2) odd?)

(5 8 (1 2) 6)

> (list-set ’(5 8 7 6) 2 ’(1 2) even?)

(5 8 7 6)

> (list-set ’(5 8 7 6) 3 ’(1 5 10) mayor5? )

(5 8 7 (1 5 10) )

> (list-set ’(5 8 7 6) 0 ’(1 5 10) mayor5? )

(5 8 7 6)

4. (4.5pts)Elabore una funci ́on llamadafilter-in que debe recibir dos ar-
gumentos: un predicadoPy una listaL. La funci ́on retorna una lista que
contiene los elementos que pertenecen aLy que satisfacen el predicadoP.
Ejemplos:

> (filter-in number? ’(a 2 (1 3) b 7))

(2 7)

> (filter-in symbol? ’(a (b c) 17 foo))

(a foo)

> (filter-in string? ’(a b u "univalle" "racket" "flp" 28 90 (1 2 3)))

("univalle" "racket" "flp")

5. (4.5pts) Elabore una funci ́on llamada (palindrome? palabra) que
determine si una palabra es pal ́ındromo. La funci ́on recibe como par ́ametro
una lista de s ́ımbolos o caracteres palabray retorna #t si la palabra se
lee igual de izquierda a derecha que de derecha a izquierda, y#fen caso
contrario. La soluci ́on debe realizarse de manera recursiva.
Ejemplos:

> (palindrome? ’(r a d a r))

#t

> (palindrome? ’(n e u q u e n))

#t

> (palindrome? ’(h o l a))

#f

6. (4.5pts) Elabore una funcion llamada swapper que recibe 3 argumentos:
un elemento E1, otro elemento E2, y una lista L. La funcion retorna una lista
similar a L, solo que cada ocurrencia anterior de E1 sera reemplazada por E2
y cada ocurrencia anterior de E2 sera reemplazada por E1 (Los elementos
E1 y E2 deben pertenecer a L). Ejemplos:

> (swapper ’a ’d ’(a b c d))

(d b c a)

> (swapper ’a ’d ’(a d () c d))

(d a () c a)

> (swapper ’x ’y ’(y y x y x y x x y))

(x x y x y x y y x)

7. (4.5pts) Elabore una funcion llamada cartesian-product que recibe como
argumentos 2 listas de simbolos sin repeticiones L1 y L2. La funcion debe
retornar una lista de tuplas que representen el producto cartesiano entre L1
y L2. Los pares pueden aparecer en cualquier orden. Ejemplos:

> (cartesian-product ’(a b c) ’(x y))

((a x) (a y) (b x) (b y) (c x) (c y))

> (cartesian-product ’(p q r) ’(5 6 7))

((p 5) (p 6) (p 7) (q 5) (q 6) (q 7) (r 5) (r 6) (r 7))
