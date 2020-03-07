require "substitution.k"

module PURE-SYNTAX
    imports DOMAINS-SYNTAX
    imports SUBSTITUTION

    syntax PSort ::= "$Type" [token]
                   | "$Kind" [token]

    syntax Type ::= PSort
                  | "bool"
                  | type(KVar)
                  | "Pi" KVar ":" Term "." Term

    syntax Term ::= Type
                  | KVar
                  | "lambda" KVar ":" Term "." Term [binder]
                  | Term Term [left, strict]
                  | "(" Term ")" [bracket]
endmodule

module PURE
    imports DOMAINS
    imports SUBSTITUTION

    imports PURE-SYNTAX

    syntax KVariable ::= KVar
    syntax KResult ::= Type

    configuration
    <T>
        <k> $PGM:Term </k>
        <typeEnv> .Map </typeEnv>
    </T>

    syntax KItem ::= setTEnv(Map)
                   | unify(Term, Type) [strict(1)]
                   | wellSorted(Type, PSort)
                   | ruleAxiom(PSort, PSort, PSort)

    rule unify(T, T) => .

    rule <k> setTEnv(TEnv) => . ...</k>
         <typeEnv> _ => TEnv </typeEnv>

    // START AXIOMS
    rule wellSorted(bool, $Type) => .
    rule wellSorted($Type, $Kind) => .

    rule ruleAxiom($Type, $Type, $Type) => .
    rule ruleAxiom($Type, $Kind, $Kind) => .
    // END AXIOMS

    rule <k> wellSorted(Pi X : T1 . T2, S3) =>
             wellSorted(T2, ?S2) ~> setTEnv(Env) ~>
             wellSorted(T1, ?S1) ~>
             ruleAxiom(?S1, ?S2, S3) ... </k>
         <typeEnv> Env => Env[X <- T1] </typeEnv>

    // VAR
    rule <k> X => wellSorted(T, ?S) ~> T ... </k>
         <typeEnv> ... (X |-> T) ... </typeEnv>

    // ABS
    rule <k> lambda X : T . E =>
             unify(E, ?Ret) ~> setTEnv(Env) ~>
             wellSorted(Pi X : T . ?Ret, ?S) ~>
             Pi X : T . ?Ret ... </k>
         <typeEnv> Env => Env[X <- T] </typeEnv>

    // APP
    rule M N =>
         unify(M, Pi ?X : ?T1 . ?T2) ~>
         unify(N, ?T1) ~>
         ?T2[N/?X]

    // Beta reduction
    // syntax Term ::= Term "-->*" Term
endmodule

