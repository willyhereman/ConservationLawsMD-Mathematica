(* d_roshy.m *)
(* Menu item 1-13 *)

(* Last Updated:  14 May, 2007, 13:25 by DP at CSM *)

(***  ROSENAU-HYMAN EQUATION  ***)

eq[1] = D[u[1][x,t],t]+u[1][x,t]*D[u[1][x,t],{x,3}] +
        aa*u[1][x,t]*D[u[1][x,t],x]+3*D[u[1][x,t],x]*D[u[1][x,t],{x,2}];

diffFunctionListINPUT = {eq[1]};
numDependentVariablesINPUT = 1;
independentVariableListINPUT = {x};
nameINPUT = "Rosenau-Hyman Equation";

parametersINPUT = {};
weightedParametersINPUT = {aa};

userWeightRulesINPUT = {};
rankRhoINPUT = Null;
explicitIndependentVariablesInDensitiesINPUT = Null;
formRhoINPUT = {};

(* d_roshy.m *)
(* end of file *)
