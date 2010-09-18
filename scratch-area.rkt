#lang s-exp "p4p.rkt"

deffun: tryand(x) =
  and: { false, 
         /(1, x) }

tryand(0)