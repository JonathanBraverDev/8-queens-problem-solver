(define (start)
  (calcWCS (genarateRandomBoard 8 0))) ;Worst Case Scenario ;)

(define (calcWCS queensL)
  (solve queensL (- (* (length queensL) (length queensL)) (length queensL)) 0))

(define (genarateRandomBoard boardSize index)
  (cond
    ((= index boardSize) '())
    (else (cons (add1 (random boardSize)) (genarateRandomBoard boardSize (add1 index))))))

(define (genarateRandomSizedBoard) ;returns a list of queens (of random length up to 100)
   (genarateRandomBoard (add1 (random 100)) 0))
  
(define (countAttacks queensL index)
  (cond
    ((= index (length queensL)) 0)
    (else (+ (attackerCount queensL index) (countAttacks queensL (add1 index))))))

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

(define (move queensL index moveCounter) ;index=0,moveCounret=1
  (cond
    ((= index (length queensL)) '())
    ((= moveCounter (add1 (length queensL))) (move queensL (add1 index) 0)) ;for 'human' numbers (1-8 insted of 0-7)
    ((= (list-ref queensL index) moveCounter) (move queensL index (add1 moveCounter)))
    (else (cons (append (reverse (cons moveCounter (reverse (listUntill queensL index 0)))) (listFrom queensL (add1 index))) (move queensL index (add1 moveCounter))))))

(define (listUntill L index counter)
  (cond
    ((= index 0) '())
    ((= (sub1 (length L)) index) (rest L))
    ((= counter index) '())
    (else (cons (list-ref L counter) (listUntill L index (add1 counter))))))

(define (listFrom L index)
  (cond 
    ((= index 0) L)
    ((= (length L) index) '())
    (else (cons (list-ref L index) (listFrom L (add1 index))))))

(define (findBestMove queensL lowestAtacks indexBest index lastAttackCount sameAttacksCounter) ;in this case, queensL is a list of queensL (just a note)
  (cond
    ((= index (length queensL)) (sameAttack? (list-ref queensL (randomIndexFrom indexBest)) lastAttackCount sameAttacksCounter))
    ((= (countAttacks (list-ref queensL index) 0) lowestAtacks) (findBestMove queensL lowestAtacks (cons index indexBest) (add1 index) lastAttackCount sameAttacksCounter))
    ((< (countAttacks (list-ref queensL index) 0) lowestAtacks) (findBestMove queensL lowestAtacks (list index) (add1 index) lastAttackCount sameAttacksCounter))
    (else (findBestMove queensL lowestAtacks indexBest (add1 index) lastAttackCount sameAttacksCounter))))

(define (randomIndexFrom L)
  (list-ref L (random (length L))))

(define (sameAttack? queensL lastAttackCount sameAttacksCounter)
  (cond
    ((= (countAttacks queensL 0) lastAttackCount) (solve queensL lastAttackCount (add1 sameAttacksCounter)))
    (else (solve queensL (countAttacks queensL 0) 0))))

(define (solve queensL lastAttackCount sameAttacksCounter)
  (cond
    ((= sameAttacksCounter 5) (print 'failed!) (print queensL))
    ((= (countAttacks queensL 0) 0) (print 'solved) (print queensL))
    (else (findBestMove (move queensL 0 1) (add1 lastAttackCount) 0 0 lastAttackCount sameAttacksCounter))))
