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


;; Definition of member of list
;; (member? a lat) is a meber of lat?
(define member?
  (lambda (a lat)
    (cond
     ((null? lat) #f)
     (else (or (eq? (car lat) a)
               (member? a (cdr lat)))))))


;; Definition of (rember a lat) remove member a from lat
(define rember
  (lambda (a lat)
    (cond
     ((null? lat) (quote ()))
     ((eq? (car lat) a) (cdr lat))
     (else (cons (car lat)
                 (rember a (cdr lat)))))))

;; Definition of (firsts l)
;; Return the firsts of a list of lists
(define firsts
  (lambda (l)
    (cond
     ((null? l) (quote ()))
     (else (cons (car (car l))
                 (firsts (cdr l)))))))


;; Definitions of (insertR new old lat)
;; Insert new atom after the old one
(define insertR
  (lambda (new old lat)
    (cond
     ((null? lat) (quote ()))
     (else
      (cond
       ((eq? (car lat) old) (cons (car lat)  (cons new (cdr lat))))
       (else
        (cons (car lat) (insertR new old (cdr lat)))))))))

;; Definition of (insertL new old lat)
;; Insert new atom to the left of old in lat

(define insertL
  (lambda (new old lat)
    (cond
     ((null? lat) (quote ()))
     (else
      (cond
       ((eq? (car lat) old) (cons new lat))
       (else
        (cons (car lat) (insertL new old (cdr lat)))))))))

;; Definition of (subst new old lat)
;; Replace old with new in lat.
(define subst
  (lambda (new old lat)
    (cond
     ((null? lat) (quote ()))
     ((eq? old (car lat)) (cons new (cdr lat)))
     (else
      (cons (car lat) (subst new old (cdr lat)))))))

;; Definition of (subst2 new o1 o2 lat)
;; Replace the first occurence of o1 or o2 with new in lat
(define subst2
  (lambda (new o1 o2 lat)
    (cond
     ((null? lat) (quote ()))
     ((or (eq? o1 (car lat)) (eq? o2 (car lat)))
      (cons new (cdr lat)))
     (else
      (cons (car lat) (subst2 new o1 o2 (cdr lat)))))))


;; Definition of (multirember a lat)
;; Remove all occurences of a in lat
(define multirember
  (lambda (a lat)
    (cond
     ((null? lat) (quote ()))
     ((eq? a (car lat)) (multirember a (cdr lat)))
     (else
      (cons (car lat) (multirember a (cdr lat)))))))
