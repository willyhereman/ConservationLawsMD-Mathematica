(* d_hirsat.m *)
(* Menu item 1-15 *)

(* Last Updated:  14 May, 2007, 13:09 by DP at CSM *)

(***  HIROTA-SATSUMA SYSTEM  ***)

hirotsatsumrules = {aa -> 1/2};

eq[1] = D[u[1][x,t],t] - aa*D[u[1][x,t],{x,3}] -
        6*aa*u[1][x,t]*D[u[1][x,t],x] + 6*u[2][x,t]*D[u[2][x,t],x] /.
        hirotsatsumrules;

eq[2] = D[u[2][x,t],t] + D[u[2][x,t],{x,3}] + 3*u[1][x,t]*D[u[2][x,t],x] /.
        hirotsatsumrules;

diffFunctionListINPUT = {eq[1], eq[2]};
numDependentVariablesINPUT = 2;
independentVariableListINPUT = {x};
nameINPUT = "Hirota-Satsuma System";

parametersINPUT = {};
weightedParametersINPUT = {};

userWeightRulesINPUT = {};
rankRhoINPUT = Null;
explicitIndependentVariablesInDensitiesINPUT = Null;
formRhoINPUT = {};

(* d_hirsat.m *)
(* end of file *)
