(define-struct state (queen h))

(define (start)
  (define L (list (random 8) (random 8) (random 8) (random 8) (random 8) (random 8) (random 8) (random 8)))
  (printBoard L)
  (countAttacks L 0))
  
;(countAttacks '(1 2 3 4 5 6 7 8) 0)

(define (countAttacks queensL index)
  (cond
    ((= index (length queensL)) 0)
    (else (+ (attackerCount queensL index) (countAttacks queensL (add1 index))))))


(define (newEmptyBoard size) ;size is (length queensL)
  (newBoard size size))

(define (newBoard queensL index)
  (cond
    ((= width 0) '())
    (else (cons (line length) (BoardSize length (sub1 width))))))

(define (newLine width)
  (cond
    ((= width 0) '())
    (else (cons 'O (line (sub1 width))))))

(define (printBoard B)
  (cond
    ((empty? (rest B)) (print (first B)))
    (else (print (first B))
          (newline)
          (printBoard (rest B)))))

(define (attackerCount queensL queenNum)
  (+ (countDiaAttacks queensL queenNum 0) (rowAttacks queensL queenNum 0)))

(define (rowAttacks queensL queenNum index)
  (cond
    ((= index (length queensL)) 0)
    ((= index queenNum) (rowAttacks queensL queenNum (add1 index)))
    ((= (list-ref queensL queenNum) (list-ref queensL index)) (add1 (rowAttacks queensL queenNum (add1 index))))
    (else (rowAttacks queensL queenNum (add1 index)))))

(define (diaAttacks queensL queenNum)
  (+ (countDiaAttacks queensL queenNum 0 0) (countDiaAttacks queensL queenNum)))

(define (countDiaAttacks queensL queenNum index)
  (cond
    ((= index (length queensL)) 0)
    ((= index queenNum) (countDiaAttacks queensL queenNum (add1 index)))
    ((= (list-ref queensL queenNum) (+ (list-ref queensL index) (abs (- index queenNum)))) (add1 (countDiaAttacks queensL queenNum (add1 index))))
    ((= (list-ref queensL queenNum) (- (list-ref queensL index) (abs (- index queenNum)))) (add1 (countDiaAttacks queensL queenNum (add1 index))))
    (else (countDiaAttacks queensL queenNum (add1 index)))))
