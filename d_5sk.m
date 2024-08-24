(* d_5sk.m *)
(* Menu item 1-5 *)

(* Last Updated:  4 September, 2008, 10:47 by DP at CSM *)

(***  FIFTH ORDER KdV - Sawada-Kotera Case  ***)

sawadarules = {aa -> 5, bb -> 5, cc -> 5};

eq[1] = D[u[1][x,t],t] + aa*u[1][x,t]^2*D[u[1][x,t],x] +
        bb*D[u[1][x,t],x]*D[u[1][x,t],{x,2}] +
        cc*u[1][x,t]*D[u[1][x,t],{x,3}] +
        D[u[1][x,t],{x,5}] /. sawadarules;

diffFunctionListINPUT = {eq[1]};
numDependentVariablesINPUT = 1;
independentVariableListINPUT = {x};
nameINPUT = "Fifth order KdV-Sawada-Kotera Equation";

parametersINPUT = {};
weightedParametersINPUT = {};

userWeightRulesINPUT = {};
rankRhoINPUT = Null;
explicitIndependentVariablesInDensitiesINPUT = Null;
formRhoINPUT = {};

(* d_5sk.m *)
(* end of file *)
