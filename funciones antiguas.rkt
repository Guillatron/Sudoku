;Tal vez sean utiles al final, quien sabe...


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

