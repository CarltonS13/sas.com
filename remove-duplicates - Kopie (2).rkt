#lang racket
(require csc151)

(define weight (vector -1 -1 -1))

;;;Procedure 
;;;   split-data
;;;Parameters 
;;;   lst, a list
;;;Purpose 
;;;   Splits lst into a pair of lists with the car containing 2/3 of lst's elements randomly selected
;;;   and the cdr containing the remaining 1/3
;;;Produces 
;;;   split, a pair of lists
(define split-data
  (lambda (lst)
    (let kernel ([remaining lst]
                 [practice null]
                 [test null])
      (let ([rand (random 3)])
        (cond [(null? remaining)
               (cons practice test)]
              [(< rand 2)
               (kernel (cdr remaining)
                       (cons (car remaining) practice)
                       test)]
              [else
               (kernel (cdr remaining)
                       practice
                       (cons (car remaining) test))])))))
;;; Procedure 
;;;   rand-string
;;;Parameters 
;;;   range, a positive integer
;;;Purpose 
;;;   Create a string out of a random number in range 0 (inclusive) - range (exclusive)
;;;Produces 
;;;   rand-string, a numeric string
(define rand-string-num
  (lambda (range)
    (number->string (random range))))

;;; Procedure 
;;;   list-of-lists-of-rands
;;; Parameters
;;;   count, a non-negative integer
;;;   sub-count, a non-negative integer
;;;   range, a positive integer
;;; Purpose 
;;;   Create list of count lists out of sub-count random numeric strings in a range of 0 (inclusive) - range (exclusive)
;;; Produces 
;;;   result, a list of lists of strings
(define list-of-lists-of-rands
  (lambda (count sub-count range)
    (let kernel ([to-go count]
                 [so-far (list)])
      (cond
        [(zero? to-go)
         so-far]
        [else
         (kernel (decrement to-go) (cons (list-of-rands sub-count range) so-far))]))))

;;; Procedure 
;;;   list-of-rands
;;; Parameters
;;;   count, a non-negative integer
;;;   range, a positive integer
;;; Purpose 
;;;   Create list of count random numeric strings in a range of 0 (inclusive) - range (exclusive)
;;; Produces 
;;;   result, a list of strings
(define list-of-rands
  (lambda (count range)
    (let kernel ([to-go count]
                 [so-far (list)])
      (cond
        [(zero? to-go)
         so-far]
        [else
         (kernel (decrement to-go) (cons (rand-string-num range) so-far))]))))
(define utter-mess (list-of-lists-of-rands 2000 10 2000))

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
    (let kernel ([remaining (sort lis list-equal?)]
                 [sum-so-far (list)])
      (let ([cur (car remaining)]
            [left (cdr remaining)])
        (cond
          [(null? left) 
           (cons cur sum-so-far)]
          [(equal? cur (car left)) 
           (kernel left sum-so-far)]
          [else 
           (kernel left (cons cur sum-so-far))])))))
;more efficient with more duplicates
(define filter-duplicates2
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

<<<<<<< HEAD

=======
>>>>>>> 1af671631c3c9c8ec6c71dccef553f1c3fea7c12

;;; Procedure 
;;;   list->string3
;;; Parameters
;;;   lst, a list of strings
;;; Purpose 
;;;  Append all elements of lst together into one string with § as seperator
;;; Produces 
;;;   result, a strings
(define list->string2
  (lambda (lst)
    (apply string-append lst)))
(define list->string3
  (lambda (lst)
    (let kernel ([remaining (cdr lst)]
                 [so-far ""])
      (if (null? remaining)
          so-far
          (if (string? (car remaining))
              (kernel (cdr remaining) (string-append so-far " § " (car remaining)))
              (kernel (cdr remaining) (string-append so-far " § " (number->string (car remaining)))))))))

;;; Procedure 
;;;   list-equal?
;;; Parameters
;;;   lst1, a list of strings
;;;   lst2, a list of strings
;;; Purpose 
;;;  Compares lst1 and lst2 to see which one would be alphabetically first
;;; Produces 
;;;   result, a boolean value
(define list-equal?
  (lambda (lst1 lst2)
    (string-ci>=? (list->string3 lst1) (list->string3 lst2))))




;;; Procedure 
;;;   reformat-data
;;; Parameters
;;;   lst, a list of lists
;;; Purpose 
;;;  Takes out the values at index positions 3,6,9,12 in the lst and
;;;  swaps the values at index positions 1 and 2 
;;; Produces 
;;;  result, a list of list 
(define reformat-data
  (lambda (lst)
    (let kernel ([remaining lst]
                 [data-so-far '()])
      (cond [(null? remaining)
             data-so-far]
            [else
             (let ([cur (list->vector (car remaining))])
               (kernel
                (cdr remaining)
                (cons (cons (vector-ref cur 0)
                            (cons (vector-ref cur 2)
                                  (cons (vector-ref cur 1)
                                        (cons (vector-ref cur 4)
                                              (cons (vector-ref cur 5)
                                                    (cons (vector-ref cur 7)
                                                          (cons (/ (vector-ref cur 8)  -60)
                                                                (cons (vector-ref cur 10)
                                                                      (cons (/(vector-ref cur 11) 200)
                                                                            (cons (vector-ref cur 13)  '()))))))))))
                      data-so-far)))]))))

(define reformat-data-to-id--dance-name
  (lambda (lst)
    (let kernel ([remaining lst]
                 [data-so-far '()])
      (cond [(null? remaining)
             data-so-far]
            [else
             (let ([cur (list->vector (car remaining))])
               (kernel
                (cdr remaining)
                (cons (cons (vector-ref cur 0)
                            (cons (vector-ref cur 2)
                                  (cons (vector-ref cur 15) '())))
                      data-so-far)))]))))

;;; Procedure 
;;;   dump-data
;;; Parameters 
;;;   filename, a string
;;;   list-of-data, a list of lists
;;; Purpose 
;;;   Writes every list in list-of-data into the file filename in a cvs format
;;; Produces 
;;;   [No Output]
(define dump-data
  (lambda (filename list-of-data)
    (cond [(not (file-exists? filename))
           (let ([out (open-output-file filename)])
             (let kernel ([data list-of-data])
               (cond [(null? data)
                      (close-output-port out)]
                     [else
                      (let inner-kernel ([remaining (car data)])
                        (cond [(null? (cdr remaining))
                               (display (car remaining) out)
                               (newline out)]
                              [else
                               (display (car remaining) out)
                               (display "," out)
                               (inner-kernel (cdr remaining))]))
                      (kernel (cdr data))])))]
          [else
           (delete-file filename)
           (dump-data filename list-of-data)])))

;;;Procedure 
;;;   get-all-true-dancibility
;;;Parameters 
;;;   file, a string leading to a file in cvs format and formatted by reformat
;;;Purpose 
;;;   Returns a list of all dancibility values from the data in file
;;;Produces 
;;;   all-dance, a list of floats
(define get-all-true-dancibility
  (lambda (file)
    (let kernel ([remaining (read-csv-file file)]
                 [so-far null])
      (if (null? remaining)
          (reverse so-far)
          (kernel (cdr remaining) (cons (cadar remaining) so-far))))))

(define true-dancebility-practice ;all dancebility values in practice file
  (get-all-true-dancibility "C:/Users/Moriz/Documents/GitHub/sas.com/no-duplicates-reformated-practice.csv")) 

;;;Procedure 
;;;   dancibility
;;;Parameters 
;;;   weights, a list of weights
;;;   data, a list of data values
;;;Purpose 
;;;   Returns a list of all dancibility values from the data in file
;;;Produces 
;;;   all-dance, a list of floats
(define dancebility
  (lambda (weights data)
    (apply + (map * weights data))))

;;;Procedure 
;;;   dancibility
;;;Parameters 
;;;   weights, a list of weights
;;;   data, a list of data values
;;;Purpose 
;;;   returns a list of pairs of the dancibility of a song based on the weights and the true dancibility
;;;Produces 
;;;   practice-and-true-dance, a list of pairs of calculated dancibility and the value from the practice data
(define get-practice-and-true-dance;
  (let ([data (read-csv-file "C:/Users/Moriz/Documents/GitHub/sas.com/no-duplicates-reformated-practice.csv")])
    (lambda (weights)
<<<<<<< HEAD
      (let kernel ([remaining-practice data]
                   [remaining-true true-dancebility-practice]
                   [so-far null])
        (if (null? remaining-practice)
            so-far
            (kernel (cdr remaining-practice) (cdr remaining-true)
                    (cons (cons (dancebility weights (cons (list-ref (car remaining-practice) 2) (cons (list-ref (car remaining-practice) 3) (drop (car remaining-practice) 8))))
                                (car remaining-true))
                          so-far)))))))
=======
        (let kernel ([remaining-practice data]
                     [remaining-true true-dancebility-practice]
                     [so-far null])
          (if (null? remaining-practice)
              so-far
              (kernel (cdr remaining-practice) (cdr remaining-true)
                      (cons (cons (dancebility weights  (cons (list-ref (car remaining-practice) 3) (drop (car remaining-practice) 8)))
                                  (car remaining-true))
                            so-far)))))))
>>>>>>> bfdf80ff159ec304d42910adc2beefa8c1b90907

;;; Procedure 
;;;   increase-weight
;;; Parameters
;;;   amount, a non-negative number
;;; Purpose 
;;;   increases weights based on the amount using overflowing values
;;; Produces 
;;;   [no result]
(define increase-weight;increases weights based on the amount using overflowing values
  (lambda (amount)
    (let kernel ([index 0])
      (cond [(or (>= index (vector-length weight)))
             (vector->list weight)]
            [(> (+ amount (vector-ref weight index)) 1)
             (vector-set! weight index -1)
             (kernel (increment index))]
            [else
             (vector-set! weight index (+ amount (vector-ref weight index)))]))))

;;; Procedure 
;;;   test-weights-algorithm
;;; Parameters
;;;   amount, a non-negative number
;;; Purpose 
;;;   increases weights based on the amount using overflowing values (o (l-s = 1) (l-s apply +))
;;; Produces 
;;;   result, a list 
(define test-weights-algorithm;increases weights based on the amount using overflowing values (o (l-s = 1) (l-s apply +))
  (let ([last (- (vector-length weight) 1)])
    (lambda (amount)
      (let ([limiter-last (- 1 (* 1 amount))]
            [limiter-second-to (* 1 amount)])
        (let kernel ([best-so-far (cons (list 0 0 0 0 0 0 0 0) 1)])
          (increase-weight amount)
          ;           (display best-so-far)
          ;           (display "  ")
          ;           (display weight)
          ;           (newline)
          (if ((o (r-s inbetween? -3) (l-s apply +)) (vector->list weight))
              best-so-far
              (let ([current-weight (vector->list weight)])
                (if ((o (r-s inbetween? 1) (l-s apply +)) current-weight)
                    (let ([testdance (cons current-weight (average-inbounds (get-practice-and-true-dance current-weight)))])
                      (cond [(< (cdr testdance) (cdr best-so-far))
                             (kernel testdance)]
                            [else
                             (kernel best-so-far)]))
                    (kernel best-so-far)))))))))
            

;;; Procedure 
;;;   data-check-all-weights
;;; Parameters
;;;   all-weights, a list
;;; Purpose 
;;;   finds the average distance from the true dancibility of given weights
;;; Produces 
;;;   result, a list of vectors

(define data-check-all-weights; returns a list of weights and their average distance from the true dancibility
  (lambda (all-weights)
    (let kernel ([remaining all-weights]
                 [so-far null])
      (cond [(null? remaining)
             so-far]
            [else
             (kernel (cdr remaining) (cons (cons (car remaining)
                                                 (list (average-inbounds (get-practice-and-true-dance (car remaining)))))
                                           so-far))]))))
;;; Procedure 
;;;   average-inbounds
;;; Parameters
;;;   lst, a list
;;; Purpose 
;;;   finds the average distance from the true dancibility of given weights
;;; Produces 
;;;   result, a list
(define average-inbounds
  (lambda (lst)
    (/ (reduce + (map distance lst)) (length lst))))

;;; Procedure 
;;;   inbetween?
;;; Parameters
;;;   testscore, 
;;;   truescore,  
;;; Purpose 
;;;   it checks if testscore is within a diviance of 0.000002
;;; Produces 
;;;   result, a boolean
(define inbetween?
  (lambda (testscore truescore)
    (and (< testscore (+ truescore 0.000002)) (> testscore (- truescore 0.000002)))))

;;; Procedure 
;;;   distance
;;; Parameters
;;;   pair, a pair
;;; Purpose 
;;;   finds the difference between the predicted dancability and the true danceability
;;; Produces 
;;;   result, a number
(define distance
  (lambda (pair)
    (abs (- (car pair) (cdr pair)))))


;(dump-data "C:/Users/Moriz/Documents/GitHub/sas.com/no-duplicates.csv"
;           (filter-duplicates (cdr (read-csv-file "C:/Users/Moriz/Documents/GitHub/sas.com/2000data.csv"))))
;(dump-data "C:/Users/Moriz/Documents/GitHub/sas.com/no-duplicates-reformated-to-id-dance-name.csv"
;           (reformat-data-to-id--dance-name (read-csv-file "C:/Users/Moriz/Documents/GitHub/sas.com/no-duplicates.csv")))
;(dump-data "C:/Users/Moriz/Documents/GitHub/sas.com/no-duplicates-reformated.csv"
;           (reformat-data (read-csv-file "C:/Users/Moriz/Documents/GitHub/sas.com/no-duplicates.csv")))

;(define split (split-data (read-csv-file "C:/Users/Moriz/Documents/GitHub/sas.com/no-duplicates-reformated.csv")))

;(dump-data "C:/Users/Moriz/Documents/GitHub/sas.com/no-duplicates-reformated-practice.csv"
;           (car split))
;(dump-data "C:/Users/Moriz/Documents/GitHub/sas.com/no-duplicates-reformated-test.csv"
;           (cdr split))
;(lambda (one two) (< (cadr one) (cadr two)))