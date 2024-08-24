(* d_disper.m *)
(* Menu item 1-26 *)

(* Last Updated:  14 May, 2007, 13:06 by DP at CSM *)

(***  DISPERSIVE LONG WAVE EQUATIONS  ***)
 
eq[1] = D[u[1][x,t],t] + D[u[1][x,t]*u[2][x,t],x] +
             aa*D[u[2][x,t],x] + bb*D[u[2][x,t],{x,3}];

eq[2] = D[u[2][x,t],t] + u[2][x,t]*D[u[2][x,t],x] + D[u[1][x,t],x];

diffFunctionListINPUT = {eq[1], eq[2]};
numDependentVariablesINPUT = 2;
independentVariableListINPUT = {x};
nameINPUT = "Dispersive Long Wave Equations";

parametersINPUT = {bb};
weightedParametersINPUT = {aa};

userWeightRulesINPUT = {};
rankRhoINPUT = Null;
explicitIndependentVariablesInDensitiesINPUT = Null;
formRhoINPUT = {};

(* d_disper.m *)
(* end of file *)
