(* d_chemo.m *)
(* Menu item 1-37 *)

(* Last Updated:  18 June, 2008, 17:38 by DP at CSM *)

(***  CLASSICAL CHEMOTAXIS MODEL  ***)

eq[1] = D[u[1][x,t],t] - aa*D[u[1][x,t],{x,2}] +
        chi*D[u[1][x,t]*D[u[2][x,t],x],x];

eq[2] = D[u[2][x,t],t] - epsilon*D[u[2][x,t],{x,2}] - u[1][x,t] +
        cc*u[2][x,t]

diffFunctionListINPUT = {eq[1],eq[2]};
numDependentVariablesINPUT = 2;
independentVariableListINPUT = {x};
nameINPUT = "Classical Chemotaxis Model";

parametersINPUT = {chi};
weightedParametersINPUT = {epsilon,aa,cc};

userWeightRulesINPUT = {};
rankRhoINPUT = Null;
explicitIndependentVariablesInDensitiesINPUT = Null;
formRhoINPUT = {};

(* From Hillen, A Classification of Spikes and Plateaus, SIAM Review,   *)
(*        49, 2007                                                      *)

(* d_chemo.m *)
(* end of file *)
