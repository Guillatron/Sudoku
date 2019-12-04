#lang racket

(define (Sudoku tablero)
  (car tablero))
;========================ANTIGUAS=================================
;Mete en una lista los elementos de una fila y comprueba si alguno se repite (compruebaFilaAux)
(define (compruebaFila sudoku i)
  (compruebaRepetidos empty (list-ref sudoku i)))

;Mete en una lista los elementos de una columna
(define (compruebaColumnaAux sudoku i [lista '() ])
  (cond
    [(empty? sudoku) '() ]
    [else (compruebaColumnaAux (cdr sudoku) i (append lista (list (list-ref (car sudoku) i))))]))

;Comprueba si en una lista se repiten elementos distintos de 0
(define (compruebaRepetidos fila1 fila2)
  (cond
    [(empty? fila2) #t] 
    [(equal? (car fila2) 0) (compruebaRepetidos fila1 (cdr fila2))] ;Si encuentra un 0 lo omite
    [(equal? (member (car fila2) fila1) #f) (compruebaRepetidos (append fila1 (list (car fila2))) (cdr fila2))] ; Si el numero no ha ocurrido prosigue
    [else #f])) ;Si encuentra un número repetido devuelve #f



;=======================NUEVAS=======================================
(define (casilla x y sudoku)
  (list-ref (list-ref sudoku x) y ) )






;======================UTILS=========================================
;Devuelve la diferencia de conjuntos entre las dos listas; Los elementos de la primera que no estén presentes en la segunda  
(define (dif-listas l1 l2 [res '()])
  (cond
    [(empty? l1) '()]
    [(equal? (index-of l2 (car l1) ) #f) (append (list (car l1)) (dif-listas (cdr l1) l2) )]
    [else (dif-listas (cdr l1) l2)])) 




(define (getColumnaI sudoku i [res '()])
  (for/list ([e (range 1 10)])
    (append res (casilla i e sudoku))))

(define (saca lista)
  (if (equal? lista empty) '() (append (car lista) (saca (cdr lista)))))


(define (getCuadrante sudoku i)
  (saca (getCuadranteIAux sudoku i)))

(define (getCuadranteIAux sudoku i [res '()])
  (for/list ([f (range 0 3)])
    (for/list ([c (range 0 3)])
       (append res  (casilla (+ f (* 3 (quotient i 3))) (+ c(* 3 (remainder i 3))) sudoku)
    )
)))

;Devuelve todos los valores presentes en esa fila
(define (valsPresentes lista [res '()])
  (for/list ([el lista]
             #:unless (equal? el 0) ) ; Se omiten las casillas vacias
    (append res el)))

;Devuelve los valores legales para esa fila


(define (encontrarPrimerCero sudoku)
  (for/last ([i (range 1 82)]
        #:break (equal? (casilla(quotient  (- i 1) 9) (remainder (- i 1) 9) sudoku ) 0) )
        i
     ))


(encontrarPrimerCero (list
                     (list 5 1 1 0 0 0 0 0 0)
                     (list 1 2 8 4 0 0 5 0 3)
                     (list 0 0 0 2 7 0 0 0 6)
                     (list 0 0 3 0 5 2 1 9 0)
                     (list 7 0 6 0 1 0 2 0 8)
                     (list 0 1 9 7 4 0 3 0 0)
                     (list 6 0 0 0 9 4 0 0 2)
                     (list 8 0 1 0 0 6 7 5 0)
                     (list 0 0 0 0 0 0 0 0 4)
                     ))
;(valsPresentes (list 0 2 8 4 0 0 5 0 3))
(dif-listas '(1 2 3 4) '(1 2))

(saca '((0 0 0) (4 0 0) (2 7 0)))
