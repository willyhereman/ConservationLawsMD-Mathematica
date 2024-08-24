(* d_5kk.m *)
(* Menu item 1-6 *)

(* Last Updated:  4 September, 2008, 10:55 by DP at CSM *)

(***  FIFTH ORDER KdV - Kaup-Kuperschmidt Case  ***)

kauprules = {aa -> 20, bb -> 25, cc -> 10};

eq[1] = D[u[1][x,t],t] + aa*u[1][x,t]^2*D[u[1][x,t],x] +
        bb*D[u[1][x, t],x] * D[u[1][x,t],{x,2}] +
        cc*u[1][x,t]*D[u[1][x,t],{x,3}] +
        D[u[1][x,t],{x,5}] /. kauprules;

diffFunctionListINPUT = {eq[1]};
numDependentVariablesINPUT = 1;
independentVariableListINPUT = {x};
nameINPUT = "Fifth order KdV-Kaup-Kupershmidt Equation";

parametersINPUT = {};
weightedParametersINPUT = {};

userWeightRulesINPUT = {};
rankRhoINPUT = Null;
explicitIndependentVariablesInDensitiesINPUT = Null;
formRhoINPUT = {};

(* d_5kk.m *)
(* end of file *)
