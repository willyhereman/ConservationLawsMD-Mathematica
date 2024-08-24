(* d_5ito.m *)
(* Menu item 1-7 *)

(* Last Updated:  4 September, 2008, 10:59 by DP at CSM *)

(***  FIFTH ORDER KdV - Ito Case  ***)

itorules = {aa -> 2, bb -> 6, cc -> 3};

eq[1] = D[u[1][x,t],t] + aa*u[1][x,t]^2*D[u[1][x,t],x] +
        bb*D[u[1][x,t],x]*D[u[1][x,t],{x,2}] +
        cc*u[1][x,t]*D[u[1][x,t],{x,3}] +
        D[u[1][x,t],{x,5}] /. itorules;

diffFunctionListINPUT = {eq[1]};
numDependentVariablesINPUT = 1;
independentVariableListINPUT = {x};
nameINPUT = "Fifth order KdV Equation - Ito Case";

parametersINPUT = {};
weightedParametersINPUT = {};

userWeightRulesINPUT = {};
rankRhoINPUT = Null;
explicitIndependentVariablesInDensitiesINPUT = Null;
formRhoINPUT = {};

(* d_5ito.m *)
(* end of file *)
