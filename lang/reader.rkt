#lang s-exp syntax/module-reader
;; From Eli's pl language
#:language
(λ() (string->symbol (format "tonart/lang/~a" (read (current-input-port)))))

#:wrapper2
(λ(in p) (define r (p in))
         (syntax-case r ()
           [(module name lang . body)
            (datum->syntax r (list* #'module #'name #'lang #'body) r)]
           [else r]))
