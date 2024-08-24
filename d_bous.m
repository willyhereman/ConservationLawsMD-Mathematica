(* d_bous.m *)
(* Menu item 1-30 *)

(* Last Updated:  14 May, 2007, 13:06 by DP at CSM *)

(***  BOUSSINESQ SYSTEM  ***)

eq[1] = D[u[1][x,t],t] + D[u[2][x,t],x];

eq[2] = D[u[2][x,t],t] + D[beta*u[1][x,t] - 3/2*u[1][x,t]^2 -
        aa*D[u[1][x,t],{x,2}],x];

diffFunctionListINPUT = {eq[1], eq[2]};
numDependentVariablesINPUT = 2;
independentVariableListINPUT = {x};
nameINPUT = "Boussinesq System";

parametersINPUT = {aa};
weightedParametersINPUT = {beta};

userWeightRulesINPUT = {};
rankRhoINPUT = Null;
explicitIndependentVariablesInDensitiesINPUT = Null;
formRhoINPUT = {};

(* d_bous.m *)
(* end of file *)
