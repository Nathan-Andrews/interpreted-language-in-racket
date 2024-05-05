# interpreted-language-in-racket
[School Project] A interpreter in racket for a made up programming language

This was made for a CSCE 314 Programming Languages

Interpreter for MUPL (made up programming language)

Syntax:

`(var s)` is variable use

`(int n)` is a constant

`(add e1 e2)` is addition

`(fun s1 s2 e)` is a function.  In e, s1 is bound to the function itself (for recursion) and s2 is bound to the (one) argument. Also, (fun null s2 e) is allowed for representing anonymous nonrecursive functions.

`(isgreater e1 e2)` comparison

`(ifnz e1 e2 e3)` If e1 is not 0 then e2 else e3

`(call e1 e2)` a function call

`(mlet s e1 e2)` a let expression where the value resulting from evaluating e1 is bound to s in the evaluation of e2

`(apair e1 e2)` a pair-creator

`(first e1)` gets the first part of a pair

`(second e1)` gets the second part of a pair

`(munit)` holds no value, essentially `null`

`(ismunit e1)` testing for (munit)

`(closure env f)` f is mupl function (an expression made from fun) and env is an environment mapping variables to values. Closures do not and cannot appear in source programs; they only result from evaluating functions.
