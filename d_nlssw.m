(* d_nlssw.m *)
(* Menu item 1-39 *)

(* Last Updated:  14 May, 2007, 13:21 by DP at CSM *)

(***  NONLINEAR SHALLOW WATER WAVE EQUATIONS  ***)

(* u = u[1]  *)
(* h = u[2]  *)

eq[1] = D[u[2][x,t],t] + u[1][x,t]*D[u[2][x,t],x] + 
             u[2][x,t]*D[u[1][x,t],x];

eq[2] = D[u[1][x,t],t] + u[1][x,t]*D[u[1][x,t],x] + gg*D[u[2][x,t],x];

diffFunctionListINPUT = {eq[1], eq[2]};
numDependentVariablesINPUT = 2;
independentVariableListINPUT = {x};
nameINPUT = "Nonlinear Shallow Water Wave Equations";

parametersINPUT = {gg};
weightedParametersINPUT = {};

userWeightRulesINPUT = {};
explicitIndependentVariablesInDensitiesINPUT = Null;
rankRhoINPUT = Null;
formRhoINPUT = {};

(* Taken from Billingham and King: "Wave Motion", p. 269. *)

(* d_nlssw.m *)
(* end of file *)
