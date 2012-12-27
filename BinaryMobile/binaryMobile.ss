; Exercise 2.29 from SICP
; The binary mobile problem

; A binary mobile has a left and  a right branch

(define (make-mobile left right)
  (list left right))

; A branch is constructed from:
;     a length (must be a number,) and
;     a structure (may be
;          a weight, represented by a number, or
;          another mobile

(define (make-branch length structure)
  (list length structure))
                    
;;; Selectors (part a. of the problem)
; left-branch and right-branch return the branches
; branch-length and branch-structure return the components

(define (left-branch mobile) (car mobile))
     
(define (right-branch mobile) (cadr mobile))
                    
(define (branch-length branch) (car branch))

(define (branch-structure branch) (cadr branch))

;;; Total weight (part b. of the problem)
(define (total-weight struct)
  (if (number? struct)
      struct
      (+ (total-weight (branch-structure (left-branch struct)))
         (total-weight (branch-structure (right-branch struct))))))

;;; Balanced? (part c. of the problem)
; A binary mobile is balnced if:
;    it is a single weight, or
;    it is a true mobile and
;         each mobile hanging from it is balanced, and
;         the torque produced by all the weights hanging on
;         the left side equals the torque produced by all the
;         weigths on the right.
;    The torque is the length of the arm times the total weight hanging
;    from it.
                    
(define (balanced? mobile)
  (define (torque branch)
      (* (branch-length branch) (total-weight (branch-structure branch)))) 
    (if (number? mobile)  ; just a weight
        #t
        (let ((left (left-branch mobile))
              (right (right-branch mobile)))
          (and (balanced? (branch-structure left))
               (balanced? (branch-structure right))
               (= (torque left) (torque right))))))
                    