#lang scribble/base

@(require scribble/bnf)

@(define (kwd t) (italic t))
@(define (code . e) (apply tt e))
@(define (codedisp . e) (apply verbatim #:indent 2 e))
@(define (typedisp . e) (apply verbatim #:indent 2 e))

@(define (abbrev t) (smaller t))

@(define (xml) (abbrev "XML"))
@(define (sax) (abbrev "SAX"))
@(define (dom) (abbrev "DOM"))
@(define (repl) (abbrev "REPL"))
@(define (eopl) (abbrev "EOPL"))
@(define (slgc) (abbrev "SLGC"))
@(define (slsc) (abbrev "SLSC"))
@(define (slgec) (abbrev "SLGEC"))

@title{P4P: A Syntax Proposal}
@author{Shriram Krishnamurthi}

This document proposes an alternate syntax for Racket.  It reduces the parenthetical burden, makes the language appear more traditional, and respects indentation, though not in the way you think.  It does all this while retaining the essence of Racket syntax, and even offering a high degree of tool reuse.

@section{A Thought Experiment}

Consider the following Racket function definition:
@codedisp{
(define (square x)
  (* x x))
}
Imagine you were asked to consider reducing the number of parentheses in Scheme.  Well, we certainly can't remove the parentheses around the multiplication, because it has variable arity; nor those around the function header, since we wouldn't be able to tell whether @code{*} is an argument or not.  But it would appear @code{define} contains all the information we need; that is, we could write
@codedisp{
define (square x)
  (* x x)
}

Except, of course, that this has now made our language ambiguous.  If instead we were to write
@codedisp{
define (square x)
  (* x x)
 (+ x x)
}
what do we mean?  Is the addition expression a second body in the definition of @code{square}, or is it a separate top-level expression (with @code{x} unbound)?

Aha, you might think: ``Didn't he say @italic{indentation} earlier?''  And indeed, that would help us distinguish
@codedisp{
define (square x)
  (* x x)
  (+ x x)
}
from
@codedisp{
define (square x)
  (* x x)
(+ x x)
}
and decide the earlier option (where @code{(+ x x)} had uncertain indentation) was ambiguous and hence erroneous.

Except that's not what we're going to do.  Read on.

@section{Examples}

Before we dive into details, let's see some running P4P programs.  I intentionally show only code, not output.  If there is any doubt as to what these programs mean (and you aren't just trying to be ornery), P4P has failed.

First, a few variable and function definitions:
@codedisp{
defvar: m = 10
defvar: this-better-be-6 = +(1, 2, 3)
defvar: this-better-be-0 = +()
deffun: five() = 5
deffun: trpl(x) = +(x, x, x)
deffun: g(a, b, c) = +(a, b, c)
}
Commas---yes, commas! We'll have more to say about them below.

Anonymous functions:
@codedisp{
deffun: d/dx(f) =
  defvar: delta = 0.001
  fun: (x) in:
    /(-(f(+(x, delta)),
        f(x)),
      delta)
}
Conditionals:
@codedisp{
deffun: fib(n) =
  if: =(n, 0)
    1
  elif: =(n, 1)
    1
  else:
    +(fib(sub1(n)), fib(-(n, 2)))
}
Structures:
@codedisp{
defstruct: memo (key, ans)
}
Sequencing (look for @code{do:}):
@codedisp{
deffun: memoize (f) =
  defvar: memo-table = box(empty)
  fun: args in:
    defvar: lookup =
      filter(fun: (v) in:
                equal?(args, memo-key(v)),
             unbox(memo-table))
    if: empty?(lookup)
          do: (
            set-box!(memo-table,
                     cons (make-memo (args, apply(f, args)),
                           unbox(memo-table)))
            apply(f, args))
    else:
          memo-ans(first(lookup))
}
Expressions in function position:
@codedisp{
defvar: this-better-be-9 = (fun: (n) in: *(n, n))(3)
}
Local bindings:
@codedisp{
let:
  x = 3,
  y = 2
 in:
  +(x, y)
  
let*: x = 3, y = x in: +(x, y)

letrec: even = fun: (n) in: if: zero?(n) true else: odd?(sub1(n)),
        odd = fun: (n) in:
                if: zero?(n)
                  false
                else:
                  odd?(sub1(n))
 in: list(odd?(10), even?(10))
}

@section{The Central Idea}

P4P hinges on one central idea and its consequences.

First, the idea: let's get rid of implicit-@code{begin}-ness.  Where we need a variable number of terms, write a @code{do:}.  This small change suddenly eliminates the ambiguity that pervades Racket parsing and forces parentheses to clarify intent.  The odds are that the extra typing engendered by @code{do:} will be offset by the reduction in typing parentheses.

Once we have made the syntax unambiguous @italic{without the help of parentheses}, we can get rid of the parentheses themselves.  That is, keywords like @code{deffun:} are sufficient to tell us what shape of terms to expect in legal programs.  (Of course, every language construct must follow this property---even @code{do:}.)

This results in a pleasant invariant about parenthetical structure. In Racket, Scheme, and Lisp, functions are notorious for trailing off into a saga of parentheses (which in Racket are broken up by the odd square-bracket, which sometimes makes maintenance even more painful).  In P4P, the only closing parentheses are from @italic{expressions}, because the language's constructs (other than @code{do:} and its kin, which anyway use braces) do not contribute any.  Thus, the parenthetical depth is precisely the same as the function nesting depth.  For beginners, in particular, since this rarely exceeds 2--3, neither does the number of adjacent parentheses.  For instance, in Racket, a typical memoized Fibonacci function ends in
@codedisp{
        (+ (memofib (sub1 n)) (memofib (- n 2)))]))))
}
whereas the equivalent in P4P (using @code{if:}s and @code{elif:}s in place of Racket's @code{cond}) ends in
@codedisp{
        +(memofib(sub1(n)), memofib(-(n, 2))))
}
(The one parenthesis not accounted for on this line itself is the invocation of @code{memoize}.) 

Those old enough to remember Pascal will know this isn't the whole story.  Pascal enabled programming language course instructors to ask students such world-class exam questions as the value of
@codedisp{
IF false IF false THEN 2
}
(taking some liberties with Pascal syntax): the question being, of course, which @code{IF} the @code{THEN} associates with.  If you're thinking now, finally, P4P will rely on indentation, you're wrong again (in the land of the one-armed @code{IF}, the people go blind from squinting). Rather, the P4P equivalent of this expression is simply illegal.  If you want a one-armed conditional, use @code{when:} or @code{unless:}.  Students, rejoice!

@section{Design}

Now I present some design decisions and design choices.  Decisions are those I believe in and would change only under duress; choices are points of flexibility where I can be talked into alternatives.

@subsection{Decisions}

@subsubsection{Embracing Prefix}

We remain unabashedly prefix.  By doing so, we circumvent all decisions about precedence, binding, associativity, and so on. 
Some initial grumbling may ensue when confronted with code like @code{+(1, 2)}, but this seems much less strange after you have seen @code{append(list1, list2)}.
Bootstrap anyway wants students to understand that exalted @code{+} is just another operation---just like lowly @code{append}.

@subsubsection{Adopting Racket's Token Syntax}

By not permitting infix, we are free to be generous about token names: @code{append-string}, @code{overlay/xy}, and @code{d/dx} are available.  However, there is no reason to preclude @code{e^<i*pi>-1}, either.  In short, we use Racket's token syntax, which will simplify interoperation with traditional, parenthesized Racket.

@subsubsection{Keeping Parsing Predictable}

Despite the lack of parentheses, the parser is top-down and syntax-directed.  It has only one token of lookahead, in this one case: when given an identifier in expression position, it has to look ahead for a left-parenthesis to determine whether or not this is an application.  This is common in other languages too.  If the input stream (file, @repl{} interaction, etc.) ends after the identifier, P4P treats it as a variable reference.  (This ambiguity will affect tools like the kill-s-expression key-binding: if it faces an identifier, it will have to check whether the identifier is followed by an argument list.)

One potential source of ambiguity is the function position of an application being a non-identifier expression.  In such cases, the expression must be wrapped in braces.  Because the use of expressions in function positions is not common, this is a small price pay.  Note that functions passed as arguments are bound to identifiers, so they will not suffer from this burden; the problem similarly disappears if the expression were first bound to a name (which might clarify intent).

@subsubsection{Leaving the Semantics Untouched}

This is @italic{purely} about syntax.  The semantics of P4P is precisely that of Racket.  For instance, the P4P equivalent of @code{begin} currently allows only a sequence of expressions; if Racket began to permit definitions before expressions, so would P4P.  Even naming stays untouched: if tomorrow structure constructors were to no longer be preceded by @code{make-}, that would be just as true of P4P. 

@subsubsection{Attaining Arity Clarity}

Function invocations are delimited.  Therefore we neither need to a-priori fix arity nor need types to tell us what the arity will be.  Despite this, we can have functions that unambiguously consume multiple arity, just as in Racket: @code{+(1, 2)}, @code{+(1, 2, 3)}, @code{+(1)}, and @code{+()} are all legal P4P expressions with the expected meanings.

@subsubsection{Adopting Indentation Without Semantics}

I increasingly view emphasizing good indentation as critical.  In some languages, however, indentation controls semantics.  I view this as a mistake.

In P4P, instead, the semantics controls indentation: that is, each construct has indentation rules, and the parser enforces them.  However, changing the indentation of a term either leaves the program's meaning unchanged or results in a syntax error; it cannot change the meaning of the program.  I believe this delivers the advantages of enforced indentation while curbing its worst excesses.

There is a pleasant side-effect to this decision: the parser can be run in a mode where indentation-checking is simply turned off. (Obviously, this is meaningless to do in a language where indentation controls semantics.) This can be beneficial when dealing with program-generated code.  Thus, it preserves the Lisp tradition's friendliness to generated code while imposing higher standards on human programmers.

@subsubsection{Reusing the Tool Chain}

P4P is implemented entirely using existing high-level Racket tools: it is defined entirely in terms of (a particular pattern of) @code{syntax-case} and some lower-level syntax-processing primitives.  It does not define a lexer or LR-parser.  I initially viewed this as a choice, but I have come to view this as a decision: this is the best way to ensure fidelity to Racket syntax.

@subsubsection{Avoiding Optional Syntax}

P4P does not have any optional syntax.  I believe this makes it easier to teach people to program: they want clear instructions, not ``You can do this, or you can do that...you can do whatever you want!''  (If they were ready to do whatever they wanted, they wouldn't be asking you.)  These trade-offs are best left to semantic and design levels, not syntax.  The only options in P4P are thus semantic choices: e.g., you can use or leave out @code{elif:} terms in a conditional, but that is a function of your program's logic, not your syntactic whimsy.

@subsubsection{Avoiding New Spacing Conventions}

While P4P's spacing conventions can (and should) be understood in their own right, experienced Racket programmers can safely fall back on their knowledge of Racket syntax.  This, for instance, tells us that both @code{deffun: f(x) = x} and @code{deffun: f (x) = x} are valid (and so, even, is @code{deffun: f(x)= x}), but @code{deffun:f(x) = x} and @code{deffun: f(x)=x} will not have the presumed intended effect.  I do not view this as problematic: beginners (both educators and students) @italic{always} ask about spacing conventions.  Since using spaces around tokens is safe, there is an easy rule to follow, which also enhances readability.  It would help for P4P's parser to be sensitive to the presence of special tokens and build in context-sensitive checks for them (e.g., if the first token after the function header is an identifier that begins with @code{=}, this should be caught by a special error case that admonishes the user to insert a space).

@subsection{Choices}

@subsubsection{Distinguishing Keywords}

P4P uses colons at the end of keywords.  I believe the principle of distinguishing keywords is beneficial: it tells the user, ``You are about to use a construct whose basic syntax, rules of indentation, and rules of evaluation may all be different from what you expect.''  The particular choice of colon is whimsical and free to change, though it was inspired by Python's use of colons (which is somewhat different).  P4P does not prevent ordinary program variables from ending in @code{:}, though it would be silently frowning as it processed programs that took advantage of this liberty.

@subsubsection{Using Syntactic Embellishments}

There are many syntactic embellishments in P4P.
@itemlist[
          @item{@code{=} in @code{defvar:} and @code{deffun:} aren't necessary, but adding them seemed to
                immensely improve readability.  
                In particular, they emphasize the substitution nature of these definitions.}
           
          @item{There is no @code{=} in @code{fun:}; I chose @code{in:} instead.  This is because the argument list
                does not equal the body, but rather is bound in it.  The choice of @code{in} is thus not entirely whimsical,
                but is very open to improvement.}

           @item{@code{do:} could consider using braces rather than parens, if these were enforceable.
                (Semi-colons between terms in the @code{do:} will never be enforceable, but will provide a
                pleasing touch to traditionalists---who might, however, accidentally put two commands on
                one line and be surprised to find the second one does not execute.)}
          
          @item{Using the @tt{def}- prefix for the definition constructs leaves open @tt{fun:} for anonymous functions.}
          
          @item{The syntax of @tt{fun:} feels a bit naked: one needs to really understand expression-ness to understand
                (beyond indentation)
                where a function ends.  A pair of delimiters wrapping the entire body would reduce this anxiety.}
                                                                                                                 
          @item{@tt{if:} does not need any intermediate keywords at all.  In their absence, however, the programmer would
                 be reduced to counting the number of preceding expressions and checking parity to know what they were
                 looking at.  Intermediate keywords improve both readability and error-reporting (which are probably linked).}
]

@subsubsection{Handling Variterm Constructs}

Some constructs, such as Racket's @tt{cond}, @tt{begin}, and @tt{when}, contain a variable number of body terms.  This makes it challenging to keep their parsing simple and predictable.  I see two broad ways to handle these: what I call @tt{if:}-style and @tt{do:}-style.  @tt{do:}-style is the lazy option: it uses a delimiter pair (specifically, brackets) and brutally dumps the terms between the delimiters.  @tt{if:}-style instead uses carefully-designed intermediate keywords as guideposts to the parser.  The brutality of the @tt{do:}-style could be reduced by the use of intermediate keywords, but at that point the delimiters wouldn't be necessary any longer.  (They wouldn't be @italic{necessary}, but they may still be helpful, as the number or size of sub-terms grows large.)  Constructs like @code{when:}, which frequently have multiple, imperative body terms, would be better served by the brutalist style, because otherwise programmers would have to write an additional @code{do:} inside the single body term most of the time.

@subsubsection{Avoiding Closing Delimiters}

Nothing in the language design precludes closing delimiters.  However, because parsing is always predictable, there is no @italic{need} for them, either.  Offering them could improve error reporting.

@subsubsection{Not Specifying the Indentation of Parenthetical Pairs}

P4P currently does not enforce any indentation convention on parenthetical constructs.  Indeed, I wonder to what extent the Scheme antipathy towards putting closing delimiters on separate lines is because of just how many darn ones there are.  If the only closing delimiters are for constructs that need them (such as @code{do:}), it may even---gasp---be @italic{good style} to put them on distinct lines, lining up with the opening keyword.

@section{Indentation...Rules!}

There are only three indentation rules in P4P: @slsc{}, and @slgc{}, and @slgec{}.  These stand for @italic{same-line-same-column}, @italic{same-line-greater-column}, and @italic{same-line-greater-equal-column}, respectively.  As you read more about these, you may find them insufficiently restrictive.  Keep in mind that indentation rules are contravariant to language size: sub-languages (such as teaching languages) can enforce many more restrictions on lines and columns.

@slgc{} is the fundamental rule of indentation.  As the name suggests, each sub-term must be either on the same line or (if not on the same line) indented to the right from the head term.  The same-line part enables one-liners, though a teaching language might want to prevent excessively long lines---for instance, by disallowing the same-line part entirely for some constructs.  In fact, the syntactic effect of @slgc{} is a little subtle: it means the first few arguments can be on the same line as the operator, while all subsequent ones must be indented, like so:
@codedisp{
+(1, 2,
  dbl(4),
  dbl(dbl(8)))
}

@slsc{} is used more rarely, when we want rigid alignment.  Currently, only @code{if:} uses @slsc{} for its internal keywords (@code{elif:} and @code{else:}).

Finally, @slgec{} was added for internal keywords that are not the same width as the main keyword.  One might want to write
@codedisp{
let: x = 3
 in: +(x, x)
}
to line up the colons, or instead
@codedisp{
let: x = 3
in: +(x, x)
}
to keep the code from drifting rightward.  (Of course, the programmer can put the @code{in:} on the previous line, too.)  P4P sees no need to choose between these two indentation styles.  Hence @slgec{} permits an indentation of zero or more.

The decision to use @slgc{} and not @slsc{} for, say, argument lists may be surprising.  It suggests the following is considered acceptable:
@codedisp{
deffun: f (x) =
  +(dbl(dbl(x)),
   dbl(x))
}
This looks odd, but consider instead this case:
@codedisp{
defvar: mfib =
  memoize(
    fun: (n) in:
      ...
 }
In other words, when a function has ``fat'' parameters, we don't want to force rightward drift (or effectively impose shorter function names).  Thus, we only expect arguments be farther to the right than the beginning of the function name, not necessarily ``within'' the argument's parentheses.

In practice, it has proven more pleasant to impose a slightly stricter rule for @slgc{}: to demand an indentation of at least two spaces, not just one.  Two spaces increases readability (Python programmers often use four); it also means egregious
@codedisp{
deffun: f (x) =
  +(dbl(dbl(x)),
   dbl(x))
}
is illegal, and must instead be at least
@codedisp{
deffun: f (x) =
  +(dbl(dbl(x)),
    dbl(x))
}

One consequence of the relative laxness of @slgc{}---which a teaching language might want to tighten---is that P4P doesn't enforce that the immediate sub-expressions in an @code{if:} are at the same level.  Thus, both
@codedisp{
if: test1
    e1
elif: test2
    e2
else:
    e3
}
and
@codedisp{ 
if: test1
    e1
elif: test2
      e2
else:
      e3
}
are legal, as different collections of people may prefer different coding styles.

@section{On Groves and Brooks (or, Trees and Streams)}

The Lisp bicameral syntax tradition is based on processing @italic{trees}.  The parentheses chunk tokens into well-formed trees, and the parser chunks these into valid trees.  It's parentheses---and thus trees---all the way down.

Except, it isn't.  A file is not a tree.  Thus, sitting outside every Lisp parser of popular imagination is another parser that operates, instead, on @italic{streams}.

Happily, Racket provides a middle-ground: files without explicit wrappers can be written in @code{#lang}, but @code{#%module-begin} turns this back into a tree.

This mapping enables the P4P parser to leverage the Racket macro system to bootstrap.  P4P removes tokens sequentially, using a slack term in every pattern to match the rest of the stream; each construct's parser returns a tree and what remains of the stream after it is done processing.

Oh, and commas.  Of course, Racket converts commas to @code{unquote}.  In Racket, the @code{unquote} is followed by a single tree; in P4P, it is followed by an arbitrary @italic{undelimited} expression.  So P4P lets Racket turn commas into @code{unquote}s, and then simply returns the subsequent tree (in Racket's terms) to the front of the token stream, for continued P4P parsing. 

@section{Error Reporting}

I have invested (almost) no time into error messages, yet.

By being a @italic{macro} over existing Racket, P4P inherits much of Racket's context-sensitive error-reporting.
Naturally, having additional clauses in P4P can improve error checking.  For instance, in the current implementation, @code{deffun: f "var" = 3} and @code{deffun: f(3) = 3} happen to be caught by P4P itself (which highlights the appropriate term), while other errors pass through to Racket, using its error messages and highlighting.  (The expression @code{3(4)} @italic{ought} to demonstrate this, but currently fails on a internal error instead.)

Because P4P's parsing is done through streams rather than trees, it is unclear how much of Ryan Culpepper's infrastructure for strengthening tree-based patterns to insert error checks will apply here.  It is more likely that something analogous needs to be created for stream processing.  In the best case, of course, Ryan's work will carry over unchanged.  Either way, this will be a fruitful area for further examination.

Finally, one known problematic case is this: when a comma-separated list fails to have a term between the (intended) penultimate comma and the closing parentheses (e.g., @code{f(x, y,)}).  This is an unfortunate consequence of P4P's attempt to reuse the Racket toolchain, and will need special support.  This is a place where @eopl{}'s sllgen parser has no problems, because it natively implements both scanner and parser.

@section{Syntax Extensions}

It would be easy to add new constructs such as @code{or:} and @code{and:}, @code{provide:}, @code{test:}, @code{const:} (to distinguish from @code{defvar:}), and so on.

The current design of P4P also does not preclude the addition of syntactic enhancements such as type declarations, default argument values, and so on.  It is presumably also possible to add support for Racket keywords and reader extensions.

One particularly curious form of syntactic extension would be to use fully-parenthesized terms in some contexts.  For instance, we might add a @code{racket:} construct that is followed by a fully-parenthesized s-expression.  Because of the nature of P4P's syntax, this can be done without any ambiguity.  One might even, say, decide to use P4P syntax to define macros for @italic{parenthetical} Racket; the P4P versions of @code{syntax-rules} or @code{syntax-case} can exploit P4P's parenthetical sparsity @italic{except} for the patterns themselves, which would be  fully-parenthesized as they would in traditional Racket (and in the source they process).

Beyond this, it is in principle possible for developers to create macros for new P4P syntactic constructs.  After all, P4P is already defined using just macros.  However:
@itemlist[

          @item{The macro definer has to understand the stream-processing pattern, 
                which is different from traditional tree-shaped macro processing.}
                                                                        
          @item{Even more importantly, the macro writer undertakes to create a construct that does not
                introduce syntactic ambiguity---a property that is guaranteed in Racket, but earned in P4P.
                (To be clear, a new Racket macro can be ambiguous: imagine an @code{infix} macro, which
                requires precedence rules for disambiguation.  However,
                this ambiguity is limited to the inside of the new construct, and cannot affect terms past
                the closing parenthesis.  In P4P, the effect may leak past the end of the construct.)}
                                                                                                      
]
For these reasons, we will probably need to create a macro-definition facility: a @code{syntax-rules} for streams.  However, that is not enough:
@itemlist[

          @item{The macro writer needs to check indentation.  This may require a pattern language that is
                indentation-sensitive.}
                                       
          @item{The output of the macros will, by default, interact with the indentation checking of the
                underlying P4P language.  One option is to have the macros respect this, though it will likely
                make them too difficult to write (because any loss of source location would leave the underlying
                P4P parser unable to perform checks, and hence forced to reject the program).  A second option is to
                generate code in a P4P variant that doesn't check indentation.  A third, perhaps best,
                solution would be to generate Racket code directly, just as the current P4P does: that is, the
                macro system would be an attached-at-the-hip, cooperating twin of P4P, rather than a layer atop it.}

]

@section{Conclusion}

Racket has a excellent language design, a great implementation, a superb programming environment, and terrific tools.  Mainstream adoption will, however, always be curtailed by the syntax.  Racket could benefit from liposuction, stomach stapling, or just plain getting off the couch and getting out for a ride, to reduce the layers of parenthetical adipose that---as this document argues, @italic{needlessly}---engird it.  P4P is a proposal for how to do this without losing the essential nature of the Lisp syntactic heritage (and, indeed, bringing to the surface the streaming nature that has always been hidden within).
