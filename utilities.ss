; Other Utility Methods
(define implst->list
  (letrec ([loop (lambda (vars)
    (if (pair? vars)
        (cons (car vars) (loop (cdr vars)))
        (list vars)))])
  loop))

(define list-set-at-index!
  (lambda (ls ind val)
    (if (= 0 ind) (set-car! ls val)
      (list-set-at-index! (cdr ls) (- ind 1) val))))

(define implst-map ; No error checking for now
  (lambda (f ls . more)
    (letrec ([map-one-implist
            (lambda (ls)
              (cond
                [(and (not (pair? ls)) (not (null? ls))) (f ls)]
                [(null? ls) '()]
                [else (cons (f (car ls)) (map-one-implist (cdr ls)))]))])
      (if (null? more)
        (map-one-implist ls)
        (apply map map-one-implist ls more)))))