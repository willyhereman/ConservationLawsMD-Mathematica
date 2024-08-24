(* d_broer.m *)
(* Menu item 1-24 *)

(* Last Updated:  14 May, 2007, 13:06 by DP at CSM *)

(***  KAUP-BROER SYSTEM  ***)

eq[1] = D[u[1][x,t],t] - u[1][x,t]*D[u[1][x,t],x] -
        D[u[2][x,t],x] + (1/2)*D[u[1][x,t],{x,2}];

eq[2] = D[u[2][x,t],t] - D[u[1][x,t],x]*u[2][x,t] -
        u[1][x,t]*D[u[2][x,t],x] - (1/2)*D[u[2][x,t],{x,2}];

diffFunctionListINPUT = {eq[1], eq[2]};
numDependentVariablesINPUT = 2;
independentVariableListINPUT = {x};
nameINPUT = "Kaup-Broer System";

parametersINPUT = {};
weightedParametersINPUT = {};

userWeightRulesINPUT = {};
rankRhoINPUT = Null;
explicitIndependentVariablesInDensitiesINPUT = Null;
formRhoINPUT = {};

(* d_broer.m *)
(* end of file *)
