#lang s-exp "p4p.rkt"

sign: is-a-nelon? is: {Any -> Boolean}
deffun: is-a-nelon?(l) =
  not(empty?(l))
  
defsigpred: NELON = is-a-nelon?

sign: some-nelon is: {NELON}
defvar: some-nelon = list(1, 2, 3)

defsigpred: NELON2 = fun: (l) in: not(empty?(l))

sign: first-num is: {NELON2 -> Number}
deffun: first-num(a-nelon)
  first(a-nelon)

#|
Things that won't work:

define: NELON = signature: predicate: is-a-nelon?

defvar: 1stNumSig = signature: {predicate: is-a-nelon? -> Number}

sign: 1st-num is: 1stNumSig
deffun: 1st-num(a-nelon)
  first(a-nelon)
  
|#
