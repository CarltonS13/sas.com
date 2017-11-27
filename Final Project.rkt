#lang racket
(require csc151)
(require rackunit)
(require rackunit/text-ui)


(define data '((5 0.804 0.00479 0.56 0 0.164 0.11136666666666667 0.185 0.42511499999999997 0.264)
               (4 0.678 0.18 0.561 0.512 0.439 0.19413333333333332 0.0694 0.8700199999999999 0.904)
               (3 0.494 0.604 0.338 0.51 0.0922 0.25393333333333334 0.0261 0.43234 0.23)
               (2 0.838 0.0344 0.412 0.000234 0.159 0.11913333333333333 0.289 0.37522 0.173)
               (1 0.743 0.199 0.359 0.00611 0.137 0.17335 0.0794 0.800415 0.588)
               (0 0.833 0.0102 0.434 0.0219 0.165 0.14658333333333334 0.431 0.75031 0.286)))

(define data-practice (read-csv-file "/Users/carltonsegbefia/Documents/GitHub/sas.com/no-duplicates-reformated-practice.csv"))


(define one-weights (vector 1 1 1 1 1 1 1 1))

(define test-weights (list (vector 1 1 1 1 1 1 1 1)
                           (vector 2 2 2 2 2 2 2 2)
                           (vector 0.3 0.3 0.3 0.3 0.3 0.3 0.3 0.3)
                           (make-vector 8 0.1)
                           (make-vector 8 0.07)
                           (vector 0.5 0.5 0.5 0.5 0.5 0.5 0.5 0.5)
                           (vector 0.7 0.7 0.7 0.7 0.7 0.7 0.7 0.7)
                           ))


;;; Procedure:
;;;   weighted
;;; Parameters:
;;;   lst, a list
;;;   weights, a vector
;;; Purpose:
;;;   takes a lst and returns the sum after multipling the weights
;;;   by the values in positions 2 - 9 of the list and summing the
;;;   resulting values together
;;; Produces:
;;;   result, a number 
;;; Preconditions:
;;;  lst must be in the form (id danceability acousticness energy
;;;  instrumentalness liveness loudness speechiness tempo valence)
;;;  weights must be in the form of 8 elements each of a value between 0 and 1. 
;;; Postconditions:
;;;   if all elements in weights have a vector of 8 then weighted will return
;;;   the sum of all the values in lst
;;;   result <= (apply + (cddr lst))
;;;   

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
;;;   and return the difference between prediction and
;;;   true value
;;; Produces:
;;;   result, a non-negative number
;;; Preconditions:
;;;  lst must be in the form (id danceability acousticness energy
;;;  instrumentalness liveness loudness speechiness tempo valence)
;;;  weights must be in the form of 8 elements each of a value between 0 and 1. 
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
;;;   length, number of elements in data
;;; Purpose:
;;;   predicts the dancability of every song in data 
;;;   and returns the average difference
;;; Produces:
;;;   result, a non-negative number
;;; Preconditions:
;;;  data must be a list of lists in the form (id danceability acousticness energy
;;;  instrumentalness liveness loudness speechiness tempo valence)
;;;  weights must be in the form of 8 elements each of a value between 0 and 1. 
;;; Postconditions:
;;;   
;;;   
;;;

(define average-prediction-difference
  (lambda (data weights length)
    (letrec ([helper (lambda (data weights)
                       (if (null? data)
                           0
                           (+ (prediction-difference (car data) weights)
                              (helper (cdr data) weights))))])
      (/ (helper data weights) length))))

(check-equal? (average-prediction-difference data one-weights (length data))
              1.6149423333333333
              "average off")

;;; Procedure:
;;;   optimize-weights 
;;; Parameters:
;;;   weights, a list of vectors
;;;   data, a list of lists
;;; Purpose:
;;;   applies all possible weights in weights to data
;;;   and returnes the one which produces the least
;;;   average difference
;;; Produces:
;;;   result, a list of the optimum weight vector and the error at that weight
;;; Preconditions:
;;;  data must be a list of lists in the form (id danceability acousticness energy
;;;  instrumentalness liveness loudness speechiness tempo valence)
;;;  weights must be a list of vectors in the form of 8 elements
;;;  each of a value between 0 and 1.  
;;; Postconditions:
;;;   (cadr result) is the lowest possible value that be provided by the weights given
;;;   
;;;

(define optimize-weights
  (lambda (weights data)
    (let ([len (length data)])
      (let kernell ([weights weights]
                    [optimum-weight (car weights)]
                    [optimum-error (average-prediction-difference data (car weights) len)])
        (if (null? weights)
            (list optimum-weight optimum-error) 
            (let ([error (average-prediction-difference data (car weights) len)])
              (cond 
                [(< error optimum-error)
                 (kernell (cdr weights)(car weights) error) ]
                [else
                 (kernell (cdr weights) optimum-weight optimum-error)]
                )))))))


;;; Procedure:
;;;   prediction-accuracy
;;; Parameters:
;;;   data, a list of lists
;;;   weights, a vector
;;;   deviance, a non-negative integer
;;; Purpose:
;;;   predicts the dancability of every song in data 
;;;   and returns the average difference
;;; Produces:
;;;   result, a non-negative number
;;; Preconditions:
;;;  data must be a list of lists in the form (id danceability acousticness energy
;;;  instrumentalness liveness loudness speechiness tempo valence)
;;;  weights must be in the form of 8 elements each of a value between 0 and 1. 
;;; Postconditions:
;;;   
;;;   
;;;

(define prediction-accuracy
  (lambda (data weights deviance)
    (letrec ([helper (lambda (data weights count)
                       (cond [ (null? data)
                               count]
                             [ (<= (prediction-difference (car data) weights) deviance)
                               (helper (cdr data) weights (increment count))]
                             [else
                                 (helper (cdr data) weights count)]))])
      (helper data weights 0))))
      

(check-equal? (prediction-accuracy data-practice one-weights 10)
              10
              "average off")

;;; Procedure:
;;;   generate-weights
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



