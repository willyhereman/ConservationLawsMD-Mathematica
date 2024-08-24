(* d_phrsat.m *)
(* Menu item 1-14 *)

(* Last Updated:  14 May, 2007, 13:21 by DP at CSM *)

(***  HIROTA-SATSUMA SYSTEM with one parameter  ***)

eq[1] = D[u[1][x,t],t] - aa*D[u[1][x,t],{x,3}] -
        6*aa*u[1][x,t]*D[u[1][x,t],x] + 6*u[2][x,t]*D[u[2][x,t],x];

eq[2] = D[u[2][x,t],t] + D[u[2][x,t],{x,3}] + 3*u[1][x,t]*D[u[2][x,t],x];

diffFunctionListINPUT = {eq[1], eq[2]};
numDependentVariablesINPUT = 2;
independentVariableListINPUT = {x};
nameINPUT = "Hirota-Satsuma System with one parameter";

parametersINPUT = {aa};
weightedParametersINPUT = {};

userWeightRulesINPUT = {};
rankRhoINPUT = Null;
explicitIndependentVariablesInDensitiesINPUT = Null;
formRhoINPUT = {};

(* d_phrsat.m *)
(* end of file *)
