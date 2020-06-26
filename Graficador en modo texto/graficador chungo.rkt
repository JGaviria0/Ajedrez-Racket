#lang racket
;jhon Alex Gaviria Tobón
;26/05/2019

;Programa de Dr.Racket tal que al ingresar un polinomio imprime valores de "x" y "y"
;según un rango y un incremento ingresado por el usuario.

;antes de:
(displayln "Ingrese el polinomio:   ")
(define cadena (~a(read)))                ;guardamos el polinomio como cadena
(displayln "Ingrese el rango maximo:  ")
(define max (read))                       ;guardamos el rango maximo
(displayln "Ingrese el rango minimo:  ")
(define min (read))                       ;guardamos el rando minimo
(displayln "Ingrese el incremento:  ")
(define incremento (read))                ;guardamos el incremento
(newline) (newline)
(define coe "")                           ;creamos variables efimeras para los coeficientes
(define nucoe (string-length coe))   
(define pote "")                          ;creamos variables efimeras para las potencias
(define nupote (string-length pote))
(define inicio "")                       ;variable efimera para partir la cadena
(define final "")
(define aux "")                          ;creacion de cadena efimera
(define r "")
(define veccoe (vector ))   ;se guardan los valores de los coeficientes.
(define vecpote (vector ))  ;se guardan los valores de las potencias.
(define vecx (vector ))  ;se guardan los valores de x despues de las debidas operaciones.
(define vecy (vector ))  ;se guardan los valores de y despues de las debidas operaciones.
(if (equal? (substring cadena 0 1) "-")
    (set! cadena (string-append cadena "+"))           ;esta condicional me ayuda para saber si el primer caracter es un signo o no, para no ponerlo
    (set! cadena (string-append "+" cadena "+"))
)
(define longitud (string-length cadena))
(define longitudaux "")
(define vecaux (vector ))  ;se guardan los valores de x despues de su simplificacion a una escala.
(define vecauy (vector ))  ;se guardan los valores de x despues de su simplificacion a una escala.
;interfaz
(define (interfaz nu)
  (display "valor de x  ")                   ;una funcion que imprime valores fijos y llama al resto de las funciones
  (displayln "valor de y  ")
  (separacion 0 0)
  (x min r 0)
  (mostrar 0)
  (newline)
  )
;división de la cadena.
(define (separacion nu n)
(set! coe "")
(set! pote "")
  (if (=  (+ nu 1) longitud)            ;esta funcion es utilizada para generar una cadena efimera la cual va desde un signo a otro
      (set! r n)                        
      (begin
      (set! inicio (puntos nu))
      (set! final (puntos (+ 1 nu)))
      (set! aux (string-append (substring cadena  inicio final) " "))
      (set! longitudaux (string-length aux))
      (vereficarx 0 n 0)
      )
      )
  )
;
;Pontos de corte
(define (puntos nu)
      (if (or(equal? (substring cadena nu (+ nu 1)) "+")(equal? (substring cadena nu (+ nu 1)) "-"))
            nu
          (puntos (+ 1 nu))                     ;esta funcion es utilizada para encontrar los puntos de quiebre de la cadena
                                                ;en otras palabras, registra donde esta el signo y guarda ese valor
          )
      )
;existencia de X
(define (vereficarx nu n bandera)
  (if (= nu longitudaux)
      (if (= bandera 0)
                  (planb n)
                  (coeficiente 0 0 n)                 ;funcion secundaria para saber si entre signo y signo hay una x
                  )
      (if (equal? (substring aux nu (+ nu 1)) "x")
          (vereficarx (+ 1 nu) n (+ bandera 1))
          (vereficarx (+ 1 nu) n bandera)
          )
      )
  )
;¿y si no existe x?
(define (planb n)
           (set! pote (string-append pote " "))
           (set! nupote (string-length pote))         ;funcion que nos ayuda a ubicar un cero cuando es una constante (no tiene x)
           (string-set! pote (- nupote 1) #\0)
           (coeficiente 0 1 n)
      )
;extracción de coeficientes                       ;busca el coeficiente de la cadena efimera creada anteriormente
(define (coeficiente nu bandera n)
  (if (equal? (substring aux nu (+ nu 1)) "-")    ;primero busca si hay un negativo, si lo hay lo guarda
      (begin
      (set! coe (string-append coe " "))                   
      (string-set! coe 0 #\-)                 
      (coeficiente (+ 1 nu) bandera n)
      )
      (if (equal? (substring aux nu (+ nu 1)) "+")            ;luego busca si hay un inicio en suma o finaliza si es la situacion
          (coeficiente (+ 1 nu) bandera n)
          (if (number? (string->number (substring aux nu (+ nu 1))))    ;ubica el numero antes de la x
              (begin
               (set! coe (string-append coe " "))
               (set! nucoe (string-length coe))
               (string-set! coe (- nucoe 1) (string-ref aux nu))
               (coeficiente (+ 1 nu) bandera n)
              )
              (if (equal? (substring aux nu (+ nu 1)) "x")        ;ubica la x y busca numero a sus alrededores
                  (begin
                  (izquierda (- nu 1))
                  (guardarcoe n nu)
                  (alone (+ 1 nu) n)
                  
                  )
                  (if (= bandera 1)
                      (begin
                        (set! veccoe (vector-append veccoe (vector " ")))
                        (vector-set! veccoe n (string->number coe))
                        (guardarpote n nu)
                        (separacion final (+ n 1))
                        )
                      (void)
                      )
                  )
              )
          )
      )
  )
;vereficación de x con coeficiente 1
(define (izquierda nu)
  (if (number? (string->number (substring aux nu (+ nu 1))))
          (void)                                                  ;esta funcion me dice si a la izquierda de la x hay numero
          (begin                                                  ;si la respuesta es si, no hace nada
           (set! coe (string-append coe " "))                     ;si la respuesta es no, agrega un uno como coeficiente
           (set! nucoe (string-length coe))
           (string-set! coe (- nucoe 1) #\1)
          )
          )
      )
;¿y si la x no posee coeficiente?
(define (alone nu n)
  (if (number? (string->number (substring aux nu (+ nu 1))))
          (derecha nu n)
          (begin
           (set! pote (string-append pote " "))
           (set! nupote (string-length pote))                     ;busca un numero a la derecha, si existe llama a otra funcion
           (string-set! pote (- nupote 1) #\1)                    ;si no, simplemten pone un uno en la potencia
           (guardarpote n nu)
           (separacion final (+ n 1))
          )
          )
      )
;Extración de potencia
(define (derecha nu n)
      (if (equal? (substring aux nu (+ nu 1)) " ")
          (begin
            (guardarpote n nu)
            (separacion final (+ n 1))                       ;hubica la potencia y la guarda
            )
          (begin
           (set! pote (string-append pote " "))
           (set! nupote (string-length pote))
           (string-set! pote (- nupote 1) (string-ref aux nu))
           (derecha (+ nu 1) n)
          )
          )
      )
;guardar coeficientes
(define (guardarpote n nu)
  (set! vecpote (vector-append vecpote (vector " ")))      ;guarda el coeficiente
  (vector-set! vecpote n (string->number pote))
  )
;guardar potencias
(define (guardarcoe n nu)
  (set! veccoe (vector-append veccoe (vector " ")))       ;guarda la potenncia
  (vector-set! veccoe n (string->number coe))
  )
;valor para x
(define (x numero n cero)
  (if (>= numero (+ 1 max))
      (void)
      (begin                                             ;guarda los valores de x en un vector
        (guardarx cero numero)                           ;envia una funcion a encontrar valores de y
        (guardary cero (operacion numero n 0 0 0))
        (x (round (+ numero incremento)) n (+ cero 1)) 
         )
      )
  )
;guardar x
(define (guardarx cero numero)
  (set! vecx (vector-append vecx (vector " ")))            ;guarda los valores de x
  (vector-set! vecx cero numero)
  )
;guardar y
(define (guardary cero y)
  (set! vecy (vector-append vecy (vector " ")))           ;guarda los valores de y
  (vector-set! vecy cero y)
  )
;Operacion
(define (operacion numero n hechas cero mutable)
  (if (= r  hechas)
      0                                                   ;busca los valores de y dependientes de x
      (begin
        (set! mutable (* (vector-ref veccoe hechas) (expt numero (vector-ref vecpote hechas))))
        (+ mutable (operacion numero n (+ 1 hechas) cero mutable))
       )
      )
  )
;mostrar
(define (mostrar f)
  (if (= f (vector-length vecx))
      (void)
      (begin
        (display (vector-ref vecx f))
        (display "          ")                         ;parte que solo muestra en pantalla la tabla
        (display (vector-ref vecy f))
        (display "          ")
        (newline)
        (mostrar (+ 1 f))
        )
      )
  )
;regla de 3 x
(define largox "") 
(define (reglax nu vect)                                    ;la regla de 3 es utilizada para escalar los valores
  (set! largox (vector-length vecx))
        (set! vecaux (vector-append vecaux (vector " ")))
        (vector-set! vecaux nu (round (/(* 40 vect) max)))
      )  
;regla de 3 y
(define vecultimo "")
(define (reglay nu vect)
  (if (> (vector-ref vecy (- (vector-length vecy) 1))(vector-ref vecy 0))
        (set! vecultimo (vector-ref vecy (- (vector-length vecy) 1)))
        (set! vecultimo (vector-ref vecy 0))
        )
        (set! vecauy (vector-append vecauy (vector " ")))
        (vector-set! vecauy nu (round (/(* 12 vect) (abs vecultimo))))
  )
(define vecrep (vector ))
(define ultimoy (vector-length vecy))           
;escalar
(define (escalar nu)
  (set! largox (vector-length vecx))
  (if (= nu largox)
      (void)                                   ;llama las funciones de regla de 3 tanto para x como para y
      (begin
      (reglax nu (vector-ref vecx nu) )
      (reglay nu (vector-ref vecy nu) )
      (escalar (+ nu 1))
      )
      )
  )
;lo bueno
(define a1  (make-vector  81 " "))(vector-set! a1 40 "|")             
(define a2  (make-vector  81 " "))(vector-set! a2 40 "|")
(define a3  (make-vector  81 " "))(vector-set! a3 40 "|")
(define a4  (make-vector  81 " "))(vector-set! a4 40 "|")                  ;creacion de la grafica por medio de una matriz
(define a5  (make-vector  81 " "))(vector-set! a5 40 "|")
(define a6  (make-vector  81 " "))(vector-set! a6 40 "|")
(define a7  (make-vector  81 " "))(vector-set! a7 40 "|")
(define a8  (make-vector  81 " "))(vector-set! a8 40 "|")
(define a9  (make-vector  81 " "))(vector-set! a9 40 "|")
(define a10 (make-vector  81 " "))(vector-set! a10 40 "|")
(define a11 (make-vector  81 " "))(vector-set! a11 40 "|")        
(define a12 (make-vector  81 " "))(vector-set! a12 40 "|")
(define a13 (make-vector  81 "-"))(vector-set! a13 40 "|")
(define a14 (make-vector  81 " "))(vector-set! a14 40 "|")
(define a15 (make-vector  81 " "))(vector-set! a15 40 "|")
(define a16 (make-vector  81 " "))(vector-set! a16 40 "|")
(define a17 (make-vector  81 " "))(vector-set! a17 40 "|")
(define a18 (make-vector  81 " "))(vector-set! a18 40 "|")
(define a19 (make-vector  81 " "))(vector-set! a19 40 "|")
(define a20 (make-vector  81 " "))(vector-set! a20 40 "|")
(define a21  (make-vector  81 " "))(vector-set! a21 40 "|")
(define a22  (make-vector  81 " "))(vector-set! a22 40 "|")
(define a23  (make-vector  81 " "))(vector-set! a23 40 "|")
(define a24  (make-vector  81 " "))(vector-set! a24 40 "|")
(define a25  (make-vector  81 " "))(vector-set! a25 40 "|")
(define matrix
  (vector a1 a2 a3 a4 a5 a6 a7 a8 a9 a10 a11 a12 a13 a14 a15 a16 a17 a18 a19 a20 a21 a22 a23 a24 a25)
  )
;remplazar
(define (remplazar nu)                   ;dados y guardados ya los valores de "x" y "Y" se procede a cambiar la matriz
  (if (= nu  largox)
      (void)
      (begin
      (vector-set! (vector-ref matrix (+ (* -1(vector-ref vecauy nu)) 12)) (+ (vector-ref vecaux nu) 40) "+")
      (remplazar (+ nu 1))
      )
      )
  )      
;visual
(define (visual nu)
  (if (= nu 25)
      (void)
      (begin
        (columnas nu 0)             ;hubicar columnas de la matriz la cual indican el salto de linea
        (newline)
        (visual (+ nu 1))
        )
      )
  )
(define (columnas nu num)
  (if (= num 81) 
      (void)                 ;hubicar filas de la matriz e imprimir letra por letra espacio por espacio
      (begin
        (display (vector-ref (vector-ref matrix nu) num))
        (columnas nu (+ num 1)))
        )
      )


(define (union nu)
  (interfaz 0)
  (escalar 0)
  (remplazar 0)          ;funcion de union final
  (visual 0)
  )

(union 0)
