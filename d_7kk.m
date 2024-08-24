(* d_7kk.m *)
(* Menu item 1-11 *)

(* Last Updated:  17 December, 2007, 9:00 by DP at CSM *)

(***  SEVENTH ORDER KdV - Kaup-Kuperschmidt (KK) Case  ***)

kkrules = {aa -> 2016, bb -> 630, cc -> 2268, dd -> 504,
           ee -> 252, ff -> 147, gg -> 42};

eq[1] = D[u[1][x,t],t] + aa*u[1][x,t]^3*D[u[1][x,t],x] +
        bb*D[u[1][x,t],x]^3 +
        cc*u[1][x,t]*D[u[1][x,t],x]*D[u[1][x,t],{x,2}] +
        dd*u[1][x,t]^2*D[u[1][x,t],{x,3}] +
        ee*D[u[1][x,t],{x,2}]*D[u[1][x,t],{x,3}] +
        ff*D[u[1][x,t],x]*D[u[1][x,t],{x,4}] +
        gg*u[1][x,t]*D[u[1][x,t],{x,5}] +
        D[u[1][x,t],{x,7}] /. kkrules;

diffFunctionListINPUT = {eq[1]};
numDependentVariablesINPUT = 1;
independentVariableListINPUT = {x};
nameINPUT = "Seventh order Kaup-Kuperschmidt (KK) Equation";

parametersINPUT = {};
weightedParametersINPUT = {};

userWeightRulesINPUT = {};
rankRhoINPUT = Null;
explicitIndependentVariablesInDensitiesINPUT = Null;
formRhoINPUT = {};

(* d_7kk.m *)
(* end of file *)
