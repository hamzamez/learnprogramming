;; (atom? a) is a an atom?
(define atom?
  (lambda (x)
    (and (not (pair? x)) (not (null? x)))))

;; (lat? l) is l a list of atoms ?
;; the definition can be found chapter 2, page 16.
;; there is no error handling, if you don't pass a list or
;; a list of lists and error will occur.
(define lat?
  (lambda (l)
    (cond
     ((null? l) #t)
     ((atom? (car l)) (lat? (cdr l)))
     (else #f))))
