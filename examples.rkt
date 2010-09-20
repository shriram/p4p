#lang s-exp "p4p.rkt"

test: 4 =? 4

defvar: this-better-be-6 = add(1, 2, 3)
test: this-better-be-6 =? 6
defvar: this-better-be-0 = add()
test: this-better-be-0 =? 0
;; This will produce a run-time error: /()

deffun: tryand(x) =
  and: { false, 
         /(1, x) }
test: tryand(0) =? false

deffun: tryor(x) =
  or: {false, false, x}
test: tryor(false) =? false
test: tryor(true) =? true

deffun: five() = 5
deffun: six(x) = 6
deffun: iden(x) = x
deffun: dbl(x) = add(x, x)
deffun: trpl(x) = add(x, x, x)

deffun: f (x) = 
  add(dbl(dbl(x)), 
      dbl(x))

defvar: id =
  fun: (x) in: x

deffun: fact (n) =
  if: zero?(n)
        1
  else:
        mult(n, fact(sub(n, 1)))

defvar: v = id(fact(10))

defstruct: memo has: (key, ans)

deffun: memoize (f) =
  defvar: memo-table = box(empty)
  fun: args in:
    defvar: lookup =
      filter(fun: (v) in:
                equal?(args, memo-key(v)),
             unbox(memo-table))
    if: empty?(lookup)
        do: {
            set-box!(memo-table,
                     cons (make-memo (args, apply(f, args)), unbox(memo-table))),
            apply(f, args)
        }
    else:
          memo-ans(first(lookup))

deffun: fib(n) =
  if: numeq(n, 0) 
    1
  elseif: numeq(n, 1)
        1
  else:
        add(fib(sub1(n)), fib(sub(n, 2)))

defvar: mfib =
  memoize(
    fun: (n) in:
      if: numeq(n, 0)
           1
      elseif: numeq(n, 1)
           1
      else:
           +(fib(sub1(n)), fib(-(n, 2))))

defvar: this-better-be-9 = {fun: (n) in: *(n, n)}(3)

deffun: andalso (a, b) = if: a b else: false

defvar: levenshtein =
  memoize (
    fun: (s, t) in:
      if: andalso(empty?(s), empty?(t))
        0
      elseif: empty?(s)
        length(t)
      elseif: empty?(t)
        length(s)
      else:
        if: equal?(first(s), first(t))
          levenshtein(rest(s), rest(t))
        else:
          min(add1(levenshtein(rest(s), t)),
              add1(levenshtein(s, rest(t))),
              add1(levenshtein(rest(s), rest(t)))
           )
   )
    
test: levenshtein(string->list("kitten"), string->list("sitting")) =? 3
test: levenshtein(string->list("gumbo"), string->list("gambol")) =? 2
test: levenshtein(string->list("acgtacgtacgt"), string->list("acatacttgtact")) =? 4

deffun: g(a, b, c) = +(a, b, c)
test: g(1,2,3) =? 6

deffun: h args = args
defvar: k =
  fun: args in: args
defvar: p =
  fun: (a, b) in: +(a, b)
test: k() =? empty
test: k(1, 2) =? list(1, 2)
test: p(1, 2) =? 3

"next three outputs should be two procedures and an empty list"
levenshtein
k
k()
test: k() =? empty

;; d/dx : (R -> R) -> (R -> R)
deffun: d/dx(f) =
  defvar: delta = 0.001
  fun: (x) in:
    div(sub(f(add(x, delta)),
            f(x)),
        delta)

defvar: d/dx-of-square = d/dx(fun: (x) in: *(x,x))
test: round(d/dx-of-square(10)) =? 20
test: round(d/dx-of-square(25)) =? 50

"next output should be 4"
+(1,
    -(2,
      3,
      5),
  +(4,5))

defstruct: frob has: (a, 
  b)

"next output should be a procedure"
fun: (x) in:
  if: false
    1
  else: 2

test: 
  +(1, 2,
    dbl(4),
    dbl(dbl(8)))
=? 
  43

test: first(list("a", "b")) =? "a"

deffun: len(l) =
  if: empty?(l)
    0
  else:
    add1(len(rest(l)))
    
test: length(list(1,2,3)) =? 3

defstruct: pt has:
 (x,
  y)
test: make-pt(1, 2) =? make-pt(1, 2)

deffun: mymap(f, l) =
  if: empty?(l)
    empty
  else:
    cons(f(first(l)), mymap(f, rest(l)))

test: mymap(add1, list(1, 2, 3)) =? list(2, 3, 4)

defvar: l = list(1,2,3)

test:
  let:
    x = 3,
    y = 2
  in:
    +(x, y)
=? 5

test: let*: x = 3, y = x in: +(x, y) =? 6

test:
  letrec: even = ;; perversely written as a one-liner
               fun: (n) in: if: zero?(n) true else: odd?(sub1(n)),
          odd = fun: (n) in:
                  if: zero?(n)
                    false
                  else:
                    odd?(sub1(n))
  in: list(odd?(10), even?(10))
=? list(false, true)