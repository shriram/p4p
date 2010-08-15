#lang reader "pwpwp-prefix.rkt"

deffun: (five) 5
deffun: (six x) 6
defvar: m 10
deffun: (dbl x) (+ x x)
deffun: (f x)
  (+ (dbl (dbl x))
     (dbl x))
defvar: id
  fun: (x) x
deffun: (fact n)
  if: (zero? n)
        1
  else:
        (n . * . (fact (n . - . 1)))
        
defvar: v (id (fact 10))

defstruct: memo (key ans)

deffun: (memoize f)
  defvar: memo-table (box empty)
  fun: args
    defvar: lookup
      (filter fun: (v)
                (equal? args (memo-key v))
              (unbox memo-table))
    if: (empty? lookup)
          begin: (
            (set-box! memo-table 
                      (cons (make-memo args (apply f args)) (unbox memo-table)))
            (apply f args) )
    else:
          (memo-ans (first lookup))

defvar: fib
  (memoize
   fun: (n)
     if: (= n 0)
           1
     elif: (= n 1)
           1
     else:
           (+ (fib (sub1 n)) (fib (- n 2))))
