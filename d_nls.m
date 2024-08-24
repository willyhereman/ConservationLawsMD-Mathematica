(* d_nls.m *)
(* Menu item 1-18 *)

(* Last Updated:  14 May, 2007, 13:20 by DP at CSM *)

(***  NONLINEAR SCHRODINGER EQUATIONS  ***)
(***  Real and Imaginary parts         ***)

nlseqnrules = {aa -> 2};

eq[1] = D[u[1][x,t],t] - D[u[2][x,t],{x,2}] +
        aa*u[2][x,t]*(u[1][x,t]^2 + u[2][x,t]^2) /. nlseqnrules;

eq[2] = D[u[2][x,t],t]+D[u[1][x,t],{x,2}] -
        aa*u[1][x,t]*(u[1][x,t]^2 + u[2][x,t]^2) /. nlseqnrules;

diffFunctionListINPUT = {eq[1], eq[2]};
numDependentVariablesINPUT = 2;
independentVariableListINPUT = {x};
nameINPUT = "Nonlinear Schrodinger Equations: real and imaginary parts";

parametersINPUT = {};
weightedParametersINPUT = {};

userWeightRulesINPUT = {};
rankRhoINPUT = Null;
explicitIndependentVariablesInDensitiesINPUT = Null;
formRhoINPUT = {};

(* d_nls.m *)
(* end of file *)
