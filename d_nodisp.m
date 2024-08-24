(* d_nodisp.m *)
(* Menu item 1-27 *)

(* Last Updated:  14 May, 2007, 13:21 by DP at CSM *)

(***  NON-DISPERSIVE LONG WAVE EQUATIONS  ***)

eq[1] = D[u[1][x,t],t] + D[u[1][x,t]*u[2][x,t],x];

eq[2] = D[u[2][x,t],t] + D[u[2][x,t],x]*u[2][x,t] + D[u[1][x,t],x];

diffFunctionListINPUT = {eq[1], eq[2]};
numDependentVariablesINPUT = 2;
independentVariableListINPUT = {x};
nameINPUT = "Non-dispersive Long Wave Equations";

parametersINPUT = {};
weightedParametersINPUT = {};

userWeightRulesINPUT = {};
rankRhoINPUT = Null;
explicitIndependentVariablesInDensitiesINPUT = Null;
formRhoINPUT = {};

(* Taken from: Mathematics of Dispersive Water Waves                     *)
(*             Kupershmidt, 1985, Commun. Math. Phys. (99), p. 51-73.    *)

(* d_nodisp.m *)
(* end of file *)
