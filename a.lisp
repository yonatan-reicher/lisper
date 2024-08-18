((label f (lambda (a b)
    (cond
       ((eq a ()) b)
       (t (cons (car a) (f (cdr a) b)))
    )
)) (quote (1 2 3)) (quote (4 5 6)))
