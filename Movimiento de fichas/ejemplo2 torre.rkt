#lang racket
(require (lib "graphics.ss" "graphics"))
(open-graphics)
;Jhon ALex Gaviria Tobón
;08/07/2019
;vector modo texto
;EN ESTE CASO
;                   1    2    3    4    5    6    7    8
(define piezas (vector (vector  0  0  0  0  0  0  0  8);1
                       (vector  0  0  0  0  0  0  0  0);2
                       (vector  0  0  0  0  0  0  0  0);3
                       (vector  0  0  0  0  0  0  0  0);4
                       (vector  0  0  0  0  0  0  0  0);5
                       (vector  0  0  0  0  0  0  0  0);6
                       (vector  0  0  0  0  0  0  0  0);7
                       (vector  0  0  0  0  0  0  0  32);8
                       )
  )
;posicion inicial (efimero)
(define filai 0)
(define columnai 0)
(define columnaf 0)
(define filaf 0)
(define click 0)
(define click2 0)
(define nu 0)
(define esta-en-jaqueB 0)
(define esta-en-jaqueN 0)
(define ficha2 0)
(define torre-reyN 0)
(define torre-reyB 0)
(define mate-blancas 0)
(define mate-negras 0)
(define ventana1 (open-viewport "Ajedrez"  500 600))
;poner cada cuadro
(define (dibujar a b e)
  (if (> e 8)
      ((draw-rectangle ventana1) (make-posn 49 49) 401 401 "black")
      (if (> a 400)
          (dibujar 50 (+ b 50) (+ e 1))
          (begin
            (if(odd? e)
               (if(odd?(/ a 50))
                  (dibujar(+ a 50)b e)
                  (begin
                    ((draw-solid-rectangle ventana1)(make-posn a b)50 50 "Slate Blue")
                    (dibujar(+ a 50)b e))     
                  )
               (if(odd?(/ a 50))
                  (begin
                    ((draw-solid-rectangle ventana1)(make-posn a b)50 50 "Slate Blue")
                    (dibujar(+ a 50)b e))
                  (dibujar(+ a 50)b e))        
               )
            )
          )
      )
  ) 
(dibujar 50 50 1)
;poner cada imagen
(((draw-pixmap-posn "images/torre.gif" 'gif/mask) ventana1)(make-posn 400 400 ))
;posiciones de mouse
(define (inicio)
  (set! click  (get-mouse-click ventana1))
  (set! filai (-(truncate(/ (posn-y (mouse-click-posn click)) 50))1))
  (set! columnai (-(truncate(/ (posn-x (mouse-click-posn click)) 50))1))
  (if (and (> filai -1)(< filai 8)(> columnai -1)(< columnai 8)) 
           (vector-ref (vector-ref piezas filai) columnai)
           (inicio)
           )
  )
(define (final)
  (set! click2 (get-mouse-click ventana1))
  (set! filaf (- (truncate(/ (posn-y (mouse-click-posn click2)) 50))1))
  (set! columnaf (- (truncate(/ (posn-x (mouse-click-posn click2)) 50))1))
  )
;llamando (inicio)
(define (llamar ficha)
  (if (odd? nu)
      ;movimiento de Nagras
      (if (and (>= ficha 1)(<= ficha 16))
          (begin
            (final)
            (verificar filaf columnaf ficha nu)
            (set! esta-en-jaqueB 0)
            (if (= esta-en-jaqueB 1)
                (((draw-pixmap-posn "images/jaqueB.jpg") ventana1)(make-posn 50 475))
                (void))
            (llamar (inicio))
            )             
          (begin
            (llamar (inicio))
            )
          )
      ;movimiento de Blancas
      (if (and (>= ficha 17)(<= ficha 32))
              (begin
                (final)
                (verificar filaf columnaf ficha nu)
                (set! esta-en-jaqueN 0)
                (if (= esta-en-jaqueN 1)
                    (((draw-pixmap-posn "images/jaqueN.jpg") ventana1)(make-posn 50 475))
                    (void))
                (llamar (inicio))  
                )
              (begin
                (llamar (inicio))
                )
              )
          )
      )
;vereficar ficha
(define (verificar nu num ficha nuf)
  (if (odd? nuf)
      (if (Blanca? nu num)
          (cond
            
            ((or (= ficha 1)(= ficha 8)) (torre nu num ficha));torres
            
            (else  (display "ficha inexistente en el juego"))
            )
          (display "No te puedes comer a tu equipo")
          )
      (if (Negra? nu num)
          (cond
           
            ((or (= ficha 25)(= ficha 32)) (torre nu num ficha));torres
           
            (else  (display "ficha inexistente en el juego"))
            )
          (display "No te puedes comer a tu equipo")
          )
      )
  )
;¿es negra?
(define (Negra? ff cf)
  (if (or (and (>= (vector-ref (vector-ref piezas ff) cf) 1)
               (<= (vector-ref (vector-ref piezas ff) cf) 16))
          (= (vector-ref (vector-ref piezas ff) cf) 0))
      #t
      #f
      )
    )
;¿es blanca?
(define (Blanca? ff cf)
  (if (or (and (>= (vector-ref (vector-ref piezas ff) cf) 17)
               (<= (vector-ref (vector-ref piezas ff) cf) 32))
          (= (vector-ref (vector-ref piezas ff) cf) 0))
      #t
      #f
      )
    )
;torre
(define (torre ff cf ficha)
  (if (and (> ff -1)(< ff 8)
           (> cf -1)(< cf 8))
      (if (and (or (= filai ff) (= columnai cf))
               (prohibido 0 0 ff cf filai columnai))
              (begin
                (jugar ff cf ficha)
                (if (odd? nu)
                     (set! torre-reyN 1)
                     (set! torre-reyB 1)
                     )
                )
              (displayln "Movimiento invalido")
              )
      (displayln "Movimiento invalido")
      )
  )

;prohibiciones
(define (prohibido signo1 signo2 ff cf nu num)
  (set! signo1 (cond ((= 0 (- filai ff)) *)((< 0 (- filai ff)) -)((> 0 (- filai ff)) +)))
  (set! signo2 (cond ((= 0 (- columnai cf)) *)((< 0 (- columnai cf)) -)((> 0 (- columnai cf)) +)))
  (set! nu (signo1 nu 1))
  (set! num (signo2 num 1))
  (pro signo1 signo2 ff cf nu num)
  )
(define (pro signo1 signo2 ff cf nu num)
  (if (and (= ff nu) (= cf num))
      #t
      (if (equal? (vector-ref (vector-ref piezas nu) num) 0)
          (pro signo1 signo2 ff cf (signo1 nu 1) (signo2 num 1))
          #f
          )
      )
  )
     
;jugar (cambiar valores en matriz y modificar modo grafico)
(define (jugar ff cf ficha)
  (set! ficha2 (vector-ref (vector-ref piezas ff) cf))
  (vector-set! (vector-ref piezas filai) columnai  0)
  (vector-set! (vector-ref piezas ff) cf  ficha)
  (if (odd? nu)
      (begin
      (set! esta-en-jaqueN 0)
      
      (if (= esta-en-jaqueN 1)
          (begin
            (vector-set! (vector-ref piezas filai) columnai  ficha)
            (vector-set! (vector-ref piezas ff) cf  ficha2)
            )
          (begin
            ((draw-solid-rectangle ventana1)(make-posn 50 475) 400 107 "white")
            ((draw-solid-rectangle ventana1)(make-posn (* (+ 1 cf) 50)(* (+ ff 1)50))49 49 (color2))
            (((draw-pixmap-posn (usar ficha) 'gif/mask) ventana1)(make-posn (* (+ 1 cf) 50)(* (+ ff 1) 50)))
            ((draw-solid-rectangle ventana1)(make-posn (* (+ 1 columnai) 50)(* (+ filai 1) 50))49 49 (color))
            (set! nu (+ nu 2))
            )
          )
      )
      (begin
      (set! esta-en-jaqueB 0)
      
      (if (= esta-en-jaqueB 1)
          (begin
            (vector-set! (vector-ref piezas filai) columnai  ficha)
            (vector-set! (vector-ref piezas ff) cf  ficha2)
            )
          (begin
            ((draw-solid-rectangle ventana1)(make-posn 50 475) 400 107 "white")
            ((draw-solid-rectangle ventana1)(make-posn (* (+ 1 cf) 50)(* (+ ff 1)50))49 49 (color2))
            (((draw-pixmap-posn (usar ficha) 'gif/mask) ventana1)(make-posn (* (+ 1 cf) 50)(* (+ ff 1) 50)))
            ((draw-solid-rectangle ventana1)(make-posn (* (+ 1 columnai) 50)(* (+ filai 1) 50))49 49 (color))
            (set! nu (+ nu 2))
            )
          )
      )
      )
  )
;color del cuadro a poner
(define (color)
  (if (even? (+ filai columnai))
      "White"
      "Slate Blue"
      )
  )
(define (color2)
  (if (even? (+ columnaf filaf))
      "White"
      "Slate Blue"
      )
  )
;eleccion de ficha a usar
(define (usar ficha)
  (cond
    ((and (>= ficha 9) (<= ficha 16))  "images/Peon2.gif")
    ((or (= ficha 1)(= ficha 8))       "images/Torre2.gif")
    ((or (= ficha 3)(= ficha 6))       "images/Alfil2.gif")
    ((= ficha 2)                       "images/Caballo22.gif")
    ((= ficha 7)                       "images/Caballo2.gif")
    ((= ficha 4)                       "images/Dama2.gif")
    ((= ficha 5)                       "images/Rey2.gif")
    ((and (>= ficha 17) (<= ficha 24)) "images/peon.gif")
    ((or (= ficha 25)(= ficha 32))     "images/torre.gif")
    ((or (= ficha 27)(= ficha 30))     "images/alfil.gif")
    ((= ficha 26)                      "images/caballo12.gif")
    ((= ficha 31)                      "images/caballo.gif")
    ((= ficha 28)                      "images/dama.gif")
    ((= ficha 29)                      "images/rey.gif")
    (else  (display "ficha inexistente en el juego"))
    )
  )
(llamar (inicio))