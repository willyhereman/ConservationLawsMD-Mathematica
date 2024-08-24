(* d_kawa.m *)
(* Menu item 1-33 *)

(* Last Updated:  18 June, 2008, 15:26 by DP at CSM *)

(***  KAWAHARA EQUATION  ***)

eq[1] = D[u[1][x,t],t] + u[1][x,t]*D[u[1][x,t],x] +
        aa*D[u[1][x,t],{x,3}] - bb*D[u[1][x,t],{x,5}];

diffFunctionListINPUT = {eq[1]};
numDependentVariablesINPUT = 1;
independentVariableListINPUT = {x};
nameINPUT = "Kawahara Equation";

parametersINPUT = {bb};
weightedParametersINPUT = {aa};

userWeightRulesINPUT = {};
rankRhoINPUT = Null;
explicitIndependentVariablesInDensitiesINPUT = Null;
formRhoINPUT = {};

(* From Polyanin and Zaitsev, Handbook for Nonlinear PDEs, 2004, p. 632 *)

(* d_kawa.m *)
(* end of file *)
