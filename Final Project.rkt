#lang racket
(require csc151)

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
;;;   weighted-values
;;; Parameters:
;;;   parameters
;;; Purpose:
;;;   takes a lst and returns the sum after multipling the weights
;;; Produces:
;;;   product
;;; Preconditions:
;;;  
;;;   
;;; Postconditions:
;;;   
;;;   
;;;

(define weights (vector 1 1 1 1 1 1 1 1 1 1))

;;; Procedure:
;;;   optimize weights 
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
;;;   check-prediction
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



