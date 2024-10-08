'(label parse (lambda (str)
    ((lambda (parseTokens lex append lexIdent contains +Car seperators makeQuote)
        (car (car (parseTokens (lex str))))
     )
     '(lambda (tokens)
        (cond ((eq tokens '()) (cons '() '()))
              ((eq (car tokens) "(")
               ((lambda (parseListRes)
                   ((lambda (parseRestRes)
                       (cons (cons (car parseListRes) (car parseRestRes))
                             (cdr parseRestRes))
                    )
                    (parseTokens (cdr (cdr parseListRes)))
                   )
                )
                (parseTokens (cdr tokens))
               )
              )
              ((eq (car tokens) ")") (cons '() (cons ")" (cdr tokens))))
              ((eq (car tokens) "'")
               ((lambda (parseRestRes)
                   (cons (cons (makeQuote (car (car parseRestRes))) (cdr (car parseRestRes))) (cdr parseRestRes)))
                (parseTokens (cdr tokens))
               ))
              ('t ((lambda (parseRestRes)
                      (cons (cons (car tokens) (car parseRestRes)) (cdr parseRestRes))
                   )
                   (parseTokens (cdr tokens))))
        )
      )
     '(lambda (str)
         (cond
            ((eq str '()) '())
            ((eq (car str) "(") (cons "(" (lex (cdr str))))
            ((eq (car str) ")") (cons ")" (lex (cdr str))))
            ((eq (car str) "'") (cons "'" (lex (cdr str))))
            ((eq (car str) " ") (lex (cdr str)))
            ('t                 (lexIdent str))
         )
      )
     '(lambda (x y) 
         (cond ((eq x '()) y)
               ('t (cons (car x) (append (cdr x) y)))
         )
      )
     '(lambda (str)
         (cond ((eq str '())                        (cons "" (lex str)))
               ((contains seperators (car str))     (cons "" (lex str)))
               ('t (+Car (car str) (lexIdent (cdr str))))
         )
      )
     '(lambda (l x)
         (cond ((eq l '())     '())
               ((eq (car l) x) 't)
               ('t             (contains (cdr l) x)))
      )
     '(lambda (x l) (cons (+ x (car l)) (cdr l)))
     (cons "(" (cons ")" (cons " " (cons "'" '()))))
     '(lambda (e) (cons 'quote (cons e '())))
    )
))
