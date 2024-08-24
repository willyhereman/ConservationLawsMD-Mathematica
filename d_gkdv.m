(* d_gkdv.m *)
(* Menu item 1-2 *)

(* Last Updated:  4 September, 2008, 10:26 by DP at CSM *)

(***  GENERALIZED KORTEWEG-DE VRIES EQUATION  ***)

(* Taken from Miura, Korteweg-de Vries Equation and Generalizations I:  *)
(*     Remarkable Explicit Nonlinear Transformations, 1967              *)

(* n = 2 is the modified KdV equation.                                  *)
Clear[n];

eq[1] = D[u[1][x,t],t] + u[1][x,t]^n*D[u[1][x,t],x] +
        D[u[1][x,t],{x,3}] /. n -> 2;

diffFunctionListINPUT = {eq[1]};
numDependentVariablesINPUT = 1;
independentVariableListINPUT = {x};
nameINPUT = "Generalized KdV Equation";
noteINPUT = "The exponent in the term "<>
    ToString[Superscript[u, "n"], FormatType -> TraditionalForm]<>
    ToString[Subscript[u, x], FormatType -> TraditionalForm]<>
    " is set to n = "<>ToString[2]<>".";

parametersINPUT = {};
weightedParametersINPUT = {};

userWeightRulesINPUT = {};
rankRhoINPUT = Null;
explicitIndependentVariablesInDensitiesINPUT = Null;
formRhoINPUT = {};

(* A generalized density where n can be any value.                      *)
(*
Clear[n];
formRhoINPUT = {u[1][x,t] + u[1][x,t]^2 +
               u[1][x,t]^(n+2) - D[u[1][x,t],x]^2};
*)

(* REFERENCE:                                                           *)
(* R. M. Miura, Korteweg-de Vries Equation and Generalizations I:       *)
(* Remarkable Explicit Nonlinear Transformations,                       *)
(* J. Math. Phys. V. 9, (1967), pp. 1202-1204.                          *)

(* d_gkdv.m *)
(* end of file *)
