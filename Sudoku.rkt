#lang racket


(define (casilla x y sudoku)
  (list-ref (list-ref sudoku x) y ) )


;======================OPERACIONES DE CONJUNTOS=========================================
;Devuelve la diferencia de conjuntos entre las dos listas
(define (dif-listas l1 l2 [res '()])
  (for/list ([el l1]
             #:unless (not (equal? (index-of l2 el) #f)))
    (append res el)
    )
  )

;Devuelve la intersección de conjuntos entre l1 y l2
(define (inter-listas l1 l2 [res '()])
  (for/list ([el l1]
             #:unless (equal? (index-of l2 el) #f) )
    (append res  el)  
   )
 )


;======================= OBTENER FILA/COL/CUADRANTE ==================
;Devuelve en una lista la columna i-ésima del sudoku
(define (getColumnaI sudoku i [res '()])
  (for/list ([e (range  0 9)])
    (append res (casilla e i sudoku))))


;Devuelve en una lista el i-ésimo cuadrante del sudoku 
(define (getCuadranteI sudoku i [res '()])
  (saca(for/list ([f (range 0 3)])
    (for/list ([c (range 0 3)])
       (append res  (casilla (+ f (* 3 (quotient i 3))) (+ c(* 3 (remainder i 3))) sudoku))
))))

;Devuelve la i-ésima fila del sudoku
;NOTA: la función como se aprecia es trivial, existe únicamente para mejorar la legibilidad del codigo
(define (getFilaI sudoku i)
  (list-ref sudoku i))




;Hace un flatten de la lista Ej:'((0 0 0) (4 0 0) (2 7 0)) -> '(0 0 0 4 0 0 2 7 0)
(define (saca lista)
  (if (equal? lista empty) '() (append (car lista) (saca (cdr lista)))))




;Devuelve todos los valores presentes en esa fila
(define (valsPresentes lista [res '()])
  (for/list ([el lista]
             #:unless (equal? el 0) ) ; Se omiten las casillas vacias
    (append res el)))

;Devuelve los valores legales para esa fila
(define (legales_lista lista)
  (dif-listas '(1 2 3 4 5 6 7 8 9) (valsPresentes lista) ))




;Encuentra la posicion del primer hueco del Sudoku
(define (primerCero sudoku)
  (for/last ([i (range 1 82)]
        #:break (equal? (casilla(quotient  (- i 1) 9) (remainder (- i 1) 9) sudoku ) 0) )
        (list (remainder i 9) (quotient i 9))
     ))

;Recibe fila y columna y devuelve el numero de cuadrante donde se ubica
(define (cuadrante coords)
  (+ (quotient (first coords) 3) (* 3 (quotient (second coords) 3))))

;Devuelve los valores legales para una casilla
(define (legales coords sudoku)
  (inter-listas ( inter-listas (legales_lista(getColumnaI sudoku (first coords)) ) (legales_lista (getFilaI sudoku (second coords) )))
                (legales_lista(getCuadranteI sudoku (cuadrante coords))))  )

(define (completo? sudoku [res #t])
  (for/and ([x (range 0 9)])
    (for/and ([y (range 0 9)])
      (and res (equal? (legales (list x y) sudoku) '()))
 )))

;Sudoku[coords] = valor
(define (sustituye sudoku coords valor [res '()])
  (for/list ([x (range 0 9)])
    (for/list ([y (range 0 9)])
      (cond
        [(equal? (list y x) coords) (append res valor)]
        [else (append res (casilla x y sudoku))])
   ))
)


;Devuelve una lista con los sudokus que resultan de rellenar la casilla i
(define (desarrollar sudoku coords [res '()])
  (for/list ([i (legales coords sudoku)])
     (append res (sustituye sudoku coords i))
  )
)

(define (push_front elem lista)
  (reverse (append (reverse lista) (reverse elem))))

;Resuelve por anchura el sudoku
(define (anchura sudokus)
  (cond
    [(completo? (car sudokus)) (car sudokus)]
    [else (anchura (cdr (append sudokus (desarrollar (car sudokus) (primerCero (car sudokus))))))]
   )
 )

(define (profundidad sudokus)
  (cond
    [(completo? (car sudokus)) (car sudokus)]
    [else (profundidad (push_front (desarrollar (car sudokus) (primerCero (car sudokus))) (cdr sudokus) ))]
   )
 )


(define ejemplo (list
                     (list 5 0 0 0 0 0 0 0 0)
                     (list 0 2 8 4 0 0 5 0 3)
                     (list 0 0 0 2 7 0 0 0 6)
                     (list 0 0 3 0 5 2 1 9 0)
                     (list 7 0 6 0 1 0 2 0 8)
                     (list 0 1 9 7 4 0 3 0 0)
                     (list 6 0 0 0 9 4 0 0 2)
                     (list 8 0 1 0 0 6 7 5 0)
                     (list 0 0 0 0 0 0 0 0 4)
                     ))

(define ejemplo_res (list
                     (list 5 6 7 8 3 9 4 2 1)
                     (list 9 2 8 4 6 1 5 7 3)
                     (list 1 3 4 2 7 5 9 8 6)
                     (list 4 8 3 6 5 2 1 9 7)
                     (list 7 5 6 9 1 3 2 4 8)
                     (list 2 1 9 7 4 8 3 6 5)
                     (list 6 7 5 1 9 4 8 3 2)
                     (list 8 4 1 3 2 6 7 5 9)
                     (list 3 9 2 5 8 7 6 1 4)))

(anchura (list ejemplo))
(profundidad (list ejemplo))
;(legales (primerCero ejemplo) ejemplo)
;(completo? ejemplo_res)
;(primerCero ejemplo)
;(getColumnaI ejemplo 0)
;(valsPresentes (list 0 2 8 4 0 0 5 0 3))
;(dif-listas '(1 2 3 4) '(1 2))
;(legales_lista '(0 0 0 4 0 0 2 7 0))
