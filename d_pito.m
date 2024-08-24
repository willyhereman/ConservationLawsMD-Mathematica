(* d_pito.m *)
(* Menu item 1-16 *)

(* Last Updated:  14 May, 2007, 13:21 by DP at CSM *)

(***  ITO SYSTEM parameterized  ***)

eq[1] = D[u[1][x,t],t] - aa*D[u[1][x,t],{x,3}] - bb*u[1][x,t]*D[u[1][x,t],x] -
        cc*u[2][x,t]*D[u[2][x,t],x]; 

eq[2] = D[u[2][x,t],t] - dd*D[(u[1][x,t]*u[2][x,t]),x];

diffFunctionListINPUT = {eq[1], eq[2]};
numDependentVariablesINPUT = 2;
independentVariableListINPUT = {x};
nameINPUT = "Ito System (parameterized)";

parametersINPUT = {aa,bb,cc,dd};
weightedParametersINPUT = {};

userWeightRulesINPUT = {};
rankRhoINPUT = Null;
explicitIndependentVariablesInDensitiesINPUT = Null;
formRhoINPUT = {};

(* d_pito.m *)
(* end of file *)
