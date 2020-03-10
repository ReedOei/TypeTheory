require "substitution.k"

module PURE
    imports DOMAINS
    imports SUBSTITUTION

    syntax PSort ::= "$Type" [token]
                   | "$Kind" [token]

    syntax Type ::= PSort
                  | KVar
                  | "Pi" KVar ":" Term "." Term [binder]

    syntax Term ::= Type
                  | "(" Term ")" [bracket]
                  > Term Term    [left]
                  > "declare" KVar ":" Term "in" Term

    syntax KResult ::= Type

    configuration
    <T>
        <k> typeof($PGM:Term, ?T) ~> ?T </k>
        <typeEnv> .Map </typeEnv>
    </T>

    syntax KItem ::= typeof(Term, Term)

    rule <k> typeof(declare X : T in E, T2) => typeof(E, T2) ... </k>
         <typeEnv> TEnv => TEnv[X <- T] </typeEnv>

    // VAR
    rule <k> typeof(X:KVar, T) => . ... </k>
         <typeEnv> ... X |-> T ... </typeEnv>

    // APP
    syntax KItem ::= Term "=" Term
    rule T = T => .

    rule typeof(M N, T) =>
         typeof(M, Pi ?X : ?T1. ?T2) ~>
         typeof(N, ?T1) ~>
         ?T2[N/?X] = T
endmodule

