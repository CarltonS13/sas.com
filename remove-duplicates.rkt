#lang racket
(require csc151)
;Purpose Create a string out of a random number in range 0-int exclusive
(define rand-string-num
  (lambda (int)
    (number->string (random int))))
(define list-of-list-of-rans
  (lambda (count int rand)
    (let kernel ([to-go count]
                 [so-far (list)])
      (cond
        [(zero? to-go)
         so-far]
        [else
         (kernel (decrement to-go) (cons (list-of-rans int rand) so-far))]))))
(define list-of-rans
  (lambda (int rand)
    (let kernel ([to-go int]
                 [so-far (list)])
      (cond
        [(zero? to-go)
         so-far]
        [else
         (kernel (decrement to-go) (cons (rand-string-num rand) so-far))]))))
(define utter-mess (list-of-list-of-rans 2000 10 2000))

;;; Procedure
;;;   filter-duplicates
;;; Parameters:
;;;   lis, a list 
;;; Purpose:
;;;   to remove lists from a list of list that have all of the same
;;;   elements in the same order 
;;; Produces:
;;;   newlis, a list
;;; Preconditions:
;;;   [No additonal]   
;;; Postconditions:
;;;   - newlis will only contain elements of lis 
;;;   - no two elements of newlis should contain the same
;;; values in the same order

(define filter-duplicates
  (lambda (lis)
    (let kernel ([remaining lis]
                 [sum-so-far (list)])
      (let ([duplicate? (lambda (entry)
                          (member entry sum-so-far))]
            [cur (car remaining)]
            [left (cdr remaining)])
        (cond
          [(null? left) 
           (if (duplicate? cur)
               sum-so-far
               (cons cur sum-so-far))]
          [(duplicate? cur) 
           (kernel left sum-so-far)]
          [else 
           (kernel left (cons cur sum-so-far))])))))

;(define ls< (o (section string-ci<? <>) (section cadr <>))
;(sort la
;      #:key cadr string<?)

;(equal? '("Moore" "Emily" "emoore" "4205" "MAT" "Emeritus" "CSC")  '("Moore" "Tom" "tmoore" "0000" "Statistics" "Emeritus" "MAT"))



(define filter-duplicates-carl
  (lambda (lis)
    (null)))