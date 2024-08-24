(* d_ito.m *)
(* Menu item 1-17 *)

(* Last Updated:  14 May, 2007, 13:09 by DP at CSM *)

(***  ITO SYSTEM  ***)

itosysrules = {aa -> 1, bb -> 6, cc -> 2, dd -> 2};

eq[1] = D[u[1][x,t],t] + aa*D[u[1][x,t],{x,3}] +
        bb*u[1][x,t]*D[u[1][x,t],x] +
        cc*u[2][x,t]*D[u[2][x,t],x] /. itosysrules; 

eq[2] = D[u[2][x,t],t] + dd*D[(u[1][x,t]*u[2][x,t]),x] /. itosysrules;

diffFunctionListINPUT = {eq[1], eq[2]};
numDependentVariablesINPUT = 2;
independentVariableListINPUT = {x};
nameINPUT = "Ito System";

parametersINPUT = {};
weightedParametersINPUT = {};

userWeightRulesINPUT = {};
rankRhoINPUT = Null;
explicitIndependentVariablesInDensitiesINPUT = Null;
formRhoINPUT = {};

(* d_ito.m *)
(* end of file *)
