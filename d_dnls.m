(* d_dnls.m *)
(* Menu item 1-20 *)

(* Last Updated:  14 May, 2007, 13:09 by DP at CSM *)

(***  DNLS SYSTEM  ***)

eq[1] = D[u[1][x,t],t] + 3*u[1][x,t]^2*D[u[1][x,t],x] +
        u[2][x,t]^2*D[u[1][x,t],x] + 2*u[1][x,t]*u[2][x,t]*D[u[2][x,t],x] -
        D[u[2][x,t],{x,2}]; 
        
eq[2] = D[u[2][x,t],t] + 2*u[1][x,t]*u[2][x,t]*D[u[1][x,t],x] +
        u[1][x,t]^2*D[u[2][x,t],x] + 3*u[2][x,t]^2*D[u[2][x,t],x] +
        D[u[1][x,t],{x,2}];

diffFunctionListINPUT = {eq[1], eq[2]};
numDependentVariablesINPUT = 2;
independentVariableListINPUT = {x};
nameINPUT = "DNLS System";

parametersINPUT = {};
weightedParametersINPUT = {};

userWeightRulesINPUT = {};
rankRhoINPUT = Null;
explicitIndependentVariablesInDensitiesINPUT = Null;
formRhoINPUT = {};

(* d_dnls.m *)
(* end of file *)
