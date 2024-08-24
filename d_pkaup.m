(* d_pkaup.m *)
(* Menu item 1-22 *)

(* Last Updated:  14 May, 2007, 13:25 by DP at CSM *)

(***  KAUP SYSTEM, parameterized  ***)

eq[1] = D[u[1][x,t],t] + D[gg*D[u[1][x,t],{x,2}] +
        aa*u[2][x,t]*D[u[1][x,t],x] +
        bb*u[1][x,t]*u[2][x,t]^2 + cc*u[1][x,t]^2,x];

eq[2] = D[u[2][x,t],t] + D[hh*D[u[2][x,t],{x,2}] +
        dd*u[2][x,t]*D[u[2][x,t],x] +
        ee*u[2][x,t]^3 + ff*u[1][x,t]*u[2][x,t],x];

diffFunctionListINPUT = {eq[1], eq[2]};
numDependentVariablesINPUT = 2;
independentVariableListINPUT = {x};
nameINPUT = "Kaup System, parameterized";

parametersINPUT = {hh, gg, ff, ee, dd, cc, bb, aa};
weightedParametersINPUT = {};

userWeightRulesINPUT = {};
rankRhoINPUT = Null;
explicitIndependentVariablesInDensitiesINPUT = Null;
formRhoINPUT = {};

(* d_pkaup.m *)
(* end of file *)
