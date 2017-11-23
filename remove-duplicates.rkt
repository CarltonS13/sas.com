#lang racket
(require csc151)
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
;;;Procedure 
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

;;;Procedure 
;;;   list-of-lists-of-rands
;;;Parameters
;;;   count, a non-negative integer
;;;   sub-count, a non-negative integer
;;;   range, a positive integer
;;;Purpose 
;;;   Create list of count lists out of sub-count random numeric strings in a range of 0 (inclusive) - range (exclusive)
;;;Produces 
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

;;;Procedure 
;;;   list-of-rands
;;;Parameters
;;;   count, a non-negative integer
;;;   range, a positive integer
;;;Purpose 
;;;   Create list of count random numeric strings in a range of 0 (inclusive) - range (exclusive)
;;;Produces 
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

;(define ls< (o (section string-ci<? <>) (section cadr <>))
;(sort la
;      #:key cadr string<?)

;(equal? '("Moore" "Emily" "emoore" "4205" "MAT" "Emeritus" "CSC")  '("Moore" "Tom" "tmoore" "0000" "Statistics" "Emeritus" "MAT"))

;;;Procedure 
;;;   list->string2
;;;Parameters
;;;   lst, a list of strings
;;;Purpose 
;;;  Append all elements of lst together into one string with § as seperator
;;;Produces 
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

;;;Procedure 
;;;   list-equal?
;;;Parameters
;;;   lst1, a list of strings
;;;   lst2, a list of strings
;;;Purpose 
;;;  Compares lst1 and lst2 to see which one would be alphabetically first
;;;Produces 
;;;   result, a boolean value
(define list-equal?
  (lambda (lst1 lst2)
    (string-ci>=? (list->string3 lst1) (list->string3 lst2))))

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