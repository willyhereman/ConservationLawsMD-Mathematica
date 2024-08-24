(* d_nlsalt.m *)
(* Menu item 1-19 *)

(* Last Updated:  14 May, 2007, 13:21 by DP at CSM *)

(***  NONLINEAR SCHRODINGER EQUATIONS (NLS) EQUATIONS  ***)
(***  Original & Complex Conjagate, after i is absorbed in scale on t  ***)

eq[1] = D[u[1][x,t],t] - D[u[1][x,t],{x,2}] +
        2*u[1][x,t]^2*u[2][x,t];

eq[2] = D[u[2][x,t],t] + D[u[2][x,t],{x,2}] -
        2*u[2][x,t]^2*u[1][x,t];

diffFunctionListINPUT = {eq[1], eq[2]};
numDependentVariablesINPUT = 2;
independentVariableListINPUT = {x};
nameINPUT = "Nonlinear Schrodinger Equations: equation and conjugate";

parametersINPUT = {};
weightedParametersINPUT = {};

userWeightRulesINPUT = {};
rankRhoINPUT = Null;
explicitIndependentVariablesInDensitiesINPUT = Null;
formRhoINPUT = {};

(* d_nlsalt.m *)
(* end of file *)
