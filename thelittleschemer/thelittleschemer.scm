;; (atom? a) is a an atom?
(define atom?
  (lambda (x)
    (and (not (pair? x)) (not (null? x)))))
