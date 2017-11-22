#lang racket
(require csc151)
(require rackunit)
(require rackunit/text-ui)


;;; Procedure:
;;;   procedure-name
;;; Parameters:
;;;   parameters
;;; Purpose:
;;;   purpose
;;; Produces:
;;;   product
;;; Preconditions:
;;;  
;;;   
;;; Postconditions:
;;;   
;;;   
;;;


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


;;; Procedure:
;;;   clean-data 
;;; Parameters:
;;;   lis, a list in 
;;; Purpose:
;;;   Remove the fields that we don’t want to include from the data
;;;   fields such as key, duration, danceability  
;;; Produces:
;;;   newlis, a list
;;; Preconditions:
;;;   [No additonal]   
;;; Postconditions:
;;;   
;;;   
;;;

;;; Procedure:
;;;   remove-danceability
;;; Parameters:
;;;   lis, a list in 
;;; Purpose:
;;;   Remove the danceability fild from all data in the given list 
;;; Produces:
;;;   newlis, a list
;;; Preconditions:
;;;   [No additonal]   
;;; Postconditions:
;;;   
;;;   
;;;

;;; Procedure:
;;;   split-data
;;; Parameters:
;;;   list
;;;   limit
;;; Purpose:
;;;   purpose
;;; Produces:
;;;   product
;;; Preconditions:
;;;  
;;;   
;;; Postconditions:
;;;   
;;;   
;;;

;;; Procedure:
;;;   weighted
;;; Parameters:
;;;   lst, a list
;;;   weights, a vector
;;; Purpose:
;;;   takes a lst and returns the sum after multipling the weights
;;;   by the values in the list and putting it together
;;; Produces:
;;;   result, a number 
;;; Preconditions:
;;;  
;;;   
;;; Postconditions:
;;;   
;;;   
;;;

(define data '((5 0.804 0.00479 0.56 0 0.164 0.11136666666666667 0.185 0.42511499999999997 0.264)
               (4 0.678 0.18 0.561 0.512 0.439 0.19413333333333332 0.0694 0.8700199999999999 0.904)
               (3 0.494 0.604 0.338 0.51 0.0922 0.25393333333333334 0.0261 0.43234 0.23)
               (2 0.838 0.0344 0.412 0.000234 0.159 0.11913333333333333 0.289 0.37522 0.173)
               (1 0.743 0.199 0.359 0.00611 0.137 0.17335 0.0794 0.800415 0.588)
               (0 0.833 0.0102 0.434 0.0219 0.165 0.14658333333333334 0.431 0.75031 0.286)))


(define one-weights (vector 1 1 1 1 1 1 1 1))

(define weighted
  (lambda (lst weights)
    (let kernel ([index 0]
                 [so-far (cddr lst)]
                 [sum 0])
      (cond [(= index 8)
             sum]
            [else (kernel
                   (+ index 1)
                   (cdr so-far)
                   (+ sum (* (car so-far) (vector-ref weights index))))]))))

(check-equal? (weighted (car data) one-weights)
              (apply + (cddr(car data)))
              "weighted multiplies by 1 and sums")

;;; Procedure:
;;;   prediction-difference 
;;; Parameters:
;;;   lst, a list
;;;   weights, a vector
;;; Purpose:
;;;   predicts the dancability of a song
;;;   and return the difference
;;; Produces:
;;;   result, a non-negative number
;;; Preconditions:
;;;  
;;;   
;;; Postconditions:
;;;   
;;;   
;;;

(define prediction-difference
  (lambda (lst weights)
    (abs (- (weighted lst weights) (cadr lst)))))

(check-equal? (prediction-difference (car data) one-weights)
              0.9102716666666666
              "weighted multiplies by 1 and sums")

(check-equal? (prediction-difference (cadr data) one-weights)
              3.051553333333333
              "weighted multiplies by 1 and sums")


;;; Procedure:
;;;   average-prediction-difference 
;;; Parameters:
;;;   data, a list of lists
;;;   weights, a vector
;;; Purpose:
;;;   predicts the dancability of every song in data 
;;;   and returns the average difference
;;; Produces:
;;;   result, a non-negative number
;;; Preconditions:
;;;  
;;;   
;;; Postconditions:
;;;   
;;;   
;;;

(define average-prediction-difference
  (lambda (data weights)
    (/ (apply + (map (section prediction-difference <> weights) data))
       ( increment (length data)))))

(check-equal? (average-prediction-difference data one-weights)
              ( - (/ (apply + (map (o (section apply + <>) (section cddr <>) ) data)) 6)
                  (/ (apply + (map cadr data)) 6))
              "average off")

;;; Procedure:
;;;   optimize weights 
;;; Parameters:
;;;   weights, a list of lists
;;;   data, a list of lists
;;; Purpose:
;;;   applies all possible weights in weights to data
;;;   and returnes the one which produces the least
;;;   average difference
;;; Produces:
;;;   product
;;; Preconditions:
;;;  
;;;   
;;; Postconditions:
;;;   
;;;   
;;;


;;; Procedure:
;;;   predict
;;; Parameters:
;;;   parameters
;;; Purpose:
;;;   purpose
;;; Produces:
;;;   product
;;; Preconditions:
;;;  
;;;   
;;; Postconditions:
;;;   
;;;   
;;;

;;; Procedure:
;;;   check-fails
;;; Parameters:
;;;   weights
;;;   deviance
;;;   list
;;; Purpose:
;;;   purpose
;;; Produces:
;;;   product
;;; Preconditions:
;;;  
;;;   
;;; Postconditions:
;;;   
;;;   
;;;



