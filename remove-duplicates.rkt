#lang racket

(define la '(("Moore" "Emily" "emoore" "4205" "MAT" "Emeritus" "CSC")
             ("Moore" "Emily" "emoore" "4205" "MAT" "Emeritus" "CSC")
             ("Moore" "Tom" "tmoore" "0000" "Statistics" "Emeritus" "MAT")
             ("Moore" "Orless" "punny" "1234" "CSC" "Emeritus" "Fictitious" "NSF Grant")
  
             ("Moore" "Tom" "tmoore" "0000" "Statistics" "Emeritus" "MAT")))

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
