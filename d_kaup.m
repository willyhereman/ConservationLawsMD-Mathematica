(* d_kaup.m *)
(* Menu item 1-23 *)

(* Last Updated:  14 May, 2007, 13:09 by DP at CSM *)

(***  KAUP SYSTEM  ***)

eq[1] = D[u[1][x,t],t] + D[D[u[1][x,t],{x,2}]-
        3*u[2][x,t]*D[u[1][x,t],x] + 3*u[1][x,t]*u[2][x,t]^2 -
        3*u[1][x,t]^2,x];

eq[2] = D[u[2][x,t],t] + D[D[u[2][x,t],{x,2}] +
        3*u[2][x,t]*D[u[2][x,t],x] + u[2][x,t]^3 -
        6*u[1][x,t]*u[2][x,t],x];

diffFunctionListINPUT = {eq[1], eq[2]};
numDependentVariablesINPUT = 2;
independentVariableListINPUT = {x};

nameINPUT = "Kaup System";
parametersINPUT = {};
weightedParametersINPUT = {};

userWeightRulesINPUT = {};
rankRhoINPUT = Null;
explicitIndependentVariablesInDensitiesINPUT = Null;
formRhoINPUT = {};

(* d_kaup.m *)
(* end of file *)
