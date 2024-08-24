(* d_5lax.m *)
(* Menu item 1-4 *)

(* Last Updated:  4 September, 2008, 10:43 by DP at CSM *)

(***  FIFTH ORDER KdV - Lax Case  ***)

fiveLaxParameterRules = {aa -> 30, bb -> 20, cc -> 10};

eq[1] = D[u[1][x,t],t] + aa*u[1][x,t]^2*D[u[1][x,t],x] +
        bb*D[u[1][x,t],x]*D[u[1][x,t],{x,2}] +
        cc*u[1][x,t]*D[u[1][x,t],{x,3}] +
        D[u[1][x,t],{x,5}] /. fiveLaxParameterRules;

diffFunctionListINPUT = {eq[1]};
numDependentVariablesINPUT = 1;
independentVariableListINPUT = {x};
nameINPUT = "Fifth order KdV-Lax Equation";

parametersINPUT = {};
weightedParametersINPUT = {};

userWeightRulesINPUT = {};
rankRhoINPUT = Null;
explicitIndependentVariablesInDensitiesINPUT = Null;
formRhoINPUT = {};

(* d_5lax.m *)
(* end of file *)
