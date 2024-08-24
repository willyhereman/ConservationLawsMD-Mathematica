(* d_3ckdv.m *)
(* Menu item 1-28 *)

(* Last Updated:  14 May, 2007, 13:01 by DP at CSM *)

(***  THREE COMPONENT KORTEWEG-DE VRIES SYSTEM  ***)

eq[1] = D[u[1][x,t],t] - 6*u[1][x,t]*D[u[1][x,t],x] - D[u[1][x,t],{x,3}] +
        D[u[2][x,t]^2 + u[3][x,t]^2,x];

eq[2] = D[u[2][x,t],t] - 2*D[u[1][x,t]*u[2][x,t],x];

eq[3] = D[u[3][x,t],t] - 2*D[u[1][x,t]*u[3][x,t],x];

diffFunctionListINPUT = Table[eq[i], {i,1,3}];
numDependentVariablesINPUT = 3;
independentVariableListINPUT = {x};
nameINPUT = "Three Component KdV System";

parametersINPUT = {};
weightedParametersINPUT = {};

userWeightRulesINPUT = {};
rankRhoINPUT = Null;
explicitIndependentVariablesInDensitiesINPUT = Null;
formRhoINPUT = {};

(* Taken from: A coupled Korteweg-de Vries Equation with dispersion     *)
(*             Kupershmidt, 1985, J. Phys. 18A, L571-L573.              *)

(* d_3ckdv.m *)
(* end of file *)
