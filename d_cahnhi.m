(* d_cahnhi.m *)
(* Menu item 1-36 *)

(* Last Updated:  18 June, 2008, 17:38 by DP at CSM *)

(***  CAHN-HILLIARD EQUATION  ***)

eq[1] = D[u[1][x,t],t] -
        D[-epsilon^2*D[u[1][x,t],{x,2}] + u[1][x,t]^3 - bb*u[1][x,t],{x,2}];

diffFunctionListINPUT = {eq[1]};
numDependentVariablesINPUT = 1;
independentVariableListINPUT = {x};
nameINPUT = "Cahn-Hilliard Equation";

parametersINPUT = {epsilon};
weightedParametersINPUT = {bb};

userWeightRulesINPUT = {};
rankRhoINPUT = Null;
explicitIndependentVariablesInDensitiesINPUT = null;
formRhoINPUT = {};

(* From Hillen, A Classification of Spikes and Plateaus, SIAM Review,   *)
(*        49, 2007                                                      *)

(* d_cahnhi.m *)
(* end of file *)
