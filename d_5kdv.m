(* d_5kdv.m *)
(* Menu item 1-3 *)

(* Last Updated:  4 September, 10:39 by DP at CSM *)

(***  FIFTH ORDER KdV EQUATION  ***)

eq[1] = D[u[1][x,t],t] + aa*u[1][x,t]^2*D[u[1][x,t],x] +
        bb*D[u[1][x,t],x]*D[u[1][x,t],{x,2}] +
        cc*u[1][x,t]*D[u[1][x,t],{x,3}] + D[u[1][x,t],{x,5}];

(* aa, bb and cc are constant parameters *)

diffFunctionListINPUT = {eq[1]};
numDependentVariablesINPUT = 1;
independentVariableListINPUT = {x};
nameINPUT = "Fifth order KdV Equation with parameters";

parametersINPUT = {aa, bb, cc};
weightedParametersINPUT = {};

userWeightRulesINPUT = {};
rankRhoINPUT = Null;
explicitIndependentVariablesInDensitiesINPUT = Null;
formRhoINPUT = {};

(* d_5kdv.m *)
(* end of file *)
