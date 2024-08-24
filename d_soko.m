(* d_soko.m *)
(* Menu item 1-25 *)

(* Last Updated:  14 May, 2007, 13:29 by DP at CSM *)

(***  DRINFEL'D-SOKOLOV SYSTEM  ***)

(* aa = 3; bb = 2; cc = 2; dd = 1; *)

eq[1] = D[u[1][x,t],t] + aa*u[2][x, t]*D[u[2][x,t],x];

eq[2] = D[u[2][x,t],t] + bb*D[u[2][x,t],{x,3}] +
             cc*u[1][x, t]*D[u[2][x,t],x] +
             dd*D[u[1][x,t],x]*u[2][x,t];

diffFunctionListINPUT = {eq[1], eq[2]};
numDependentVariablesINPUT = 2;
independentVariableListINPUT = {x};
nameINPUT = "Drinfel'd-Sokolov System";

parametersINPUT = {aa, bb, cc, dd};
weightedParametersINPUT = {};

userWeightRulesINPUT = {};
rankRhoINPUT = Null;
explicitIndependentVariablesInDensitiesINPUT = Null;
formRhoINPUT = {}; 

(* d_soko.m *)
(* end of file *)
