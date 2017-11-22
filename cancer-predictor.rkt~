#lang racket
(require csc151)

(define raw-data (read-csv-file "/Users/carltonsegbefia/OneDrive - Grinnell College/CSC 151/cancer/breast-cancer-wisconsin.csv"))
(require rackunit)
(require rackunit/text-ui)

;takes a list and return true if the list is complete
(define data-cleaner
  (lambda (lst)
    (letrec ([last (list-ref lst 10)]
             [middle (lambda (index)
                       (let ([val (list-ref lst index)])
                         (cond
                           [(= index 10)
                            #t]
                           [(not (real? val))
                            #f]
                           [(<= 0 val 10)
                            (middle (+ index 1))]
                           [else #f])))])
      (and (= (length lst) 11)
           (middle 1)
           (or ( = 2 last) ( = 4 last))))))

(test-case
 "small lists"
 (check-equal? (data-cleaner '(1000025 5 1 1 1 20 1 3 1 1 2)) 
               #f
               "middle value above 10")
 (check-equal? (data-cleaner '(1000025 10 1 1 1 0 1 3 1 10 2))
               #t
               "middle value at 10")
 (check-equal? (data-cleaner '(1000025 5 1 1 1 0 1 3 1 1 3))
               #f
               "end is not 2 or 4")
 (check-equal? (data-cleaner '(1000025 5 1 1 1 0 1 3 1 1 2 2))
               #f
               "more than ten values"))

(define clean-data (filter data-cleaner raw-data))

;checks if list is for benign data
(define benign?
  (lambda (lst)
    (= 2 (list-ref lst 10))))

(define benign-data (filter benign? clean-data))
(define malignant-data (filter (negate benign?) clean-data))

(define weights (vector 0.9900000000000007
                        0.9900000000000007
                        0.9900000000000007
                        0.49000000000000027
                        0.9900000000000007
                        0.9900000000000007
                        0.9900000000000007
                        0.9900000000000007
                        0.9900000000000007))

;takes a lst and returns the sum after multipling the weights
(define weighted
  (lambda (lst)
    (let kernel ([index 0]
                 [so-far (cdr lst)]
                 [sum 0])
      (cond [(= index 9)
             sum]
            [else (kernel
                   (+ index 1)
                   (cdr so-far)
                   (+ sum (* (car so-far) (vector-ref weights index))))]))))

;the value which diferentiates maligiant from benign
(define limit-vector (vector 22))
(define limit (car limit-vector))

;based on weights and limt determins wheter a list is benign
(define predict
  (lambda (lst limit)
    (let ([val (weighted lst)])
      (if (< val limit)
          #t
          #f))))    

;checks if prediction was right
(define check-prediction
  (lambda (lst limit)
    (let ([truth (= 2 (list-ref lst 10))]
          [prediction (predict lst limit)])
      (cond [ (and truth prediction)
              "Actually Benign Predicted Benign"]
            [ (and (not truth) prediction)
              "Actually Malignant Predicted Benign"]
            [ (and truth (not prediction))
              "Actually Benign Predicted Malignant"]
            [ (and (not truth) (not prediction))
              "Actually Malignant Predicted Malignant"]))))


(define check-all-predictions
  (lambda (limit)
    (tally-all (map (section check-prediction <> limit) clean-data))))

;checks how many times prediction was wrong for data
(define check-fail
  (lambda (lst limit)
    (let ([truth (= 2 (list-ref lst 10))]
          [prediction (predict lst limit)])
      (cond[(and (not truth) prediction)
            1]
           [(and truth (not prediction))
            1]
           [else 0]
           ))))

(define check-all-fails
  (lambda (limit)
    (apply + (map (section check-fail <> limit) clean-data))))



(define optimum-weight
  (lambda (index limit)
    (let kernel ([optimum-weight 0.0]
                 [current-weight -1.0]
                 [current-val 1000.0])
      (vector-set! weights index current-weight)
      (cond [(> current-weight 1.0)
             optimum-weight]
            [(< current-val (check-all-fails limit))
             (kernel optimum-weight (+ current-weight 0.01) current-val)]
            [else
             (kernel current-weight (+ current-weight 0.01) (check-all-fails limit))]))))
            
                 
;(define optimum-weights (map (section optimum-weight <> 26) (iota 9)))


;Works now
(define optimum-limit
  (lambda ()
    (let kernel ([optimum-limit 0.0]
                 [current-limit 0.0]
                 [current-val 1000.0])
      (cond [(> current-limit 30.0)
             optimum-limit]
            [(<= current-val (check-all-fails current-limit))
             (kernel optimum-limit (+ current-limit 1) current-val)]
            [else
             (kernel current-limit (+ current-limit 1) (check-all-fails current-limit))]))))

; finds and sets optimum weight for a given limit 
(define optimum-weight-finder
  (lambda (limit)
    (map (section vector-set! weights <> <>) (iota 9)(map (section optimum-weight <> limit) (iota 9)))
    (check-all-fails limit)))


; For all possible limits checks for all optimum limits 
(define optimum
  (lambda ()
    (let kernel ([optimum-limit 0.0]
                 [current-limit 0.0]
                 [current-val 1000.0])
      (cond [(> current-limit 30.0)
             optimum-limit]
            [(<= current-val (optimum-weight-finder current-limit))
             (kernel optimum-limit (+ current-limit 1) current-val)]
            [else
             (kernel current-limit (+ current-limit 1) (optimum-weight-finder current-limit))]))))