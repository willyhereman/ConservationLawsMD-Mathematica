(* d_7kdv.m *)
(* Menu item 1-8 *)

(* Last Updated:  4 September, 2008, 11:06 by DP at CSM *)

(***  SEVENTH ORDER KdV EQUATION  ***)

eq[1] = D[u[1][x,t],t] + aa*u[1][x,t]^3*D[u[1][x,t],x] +
        bb*D[u[1][x,t],x]^3 +
        cc*u[1][x,t]*D[u[1][x,t],x]*D[u[1][x,t],{x,2}] +
        dd*u[1][x,t]^2*D[u[1][x,t],{x,3}] +
        ee*D[u[1][x,t],{x,2}]*D[u[1][x,t],{x,3}] +
        ff*D[u[1][x,t],x]*D[u[1][x,t],{x,4}] +
        gg*u[1][x,t]*D[u[1][x,t],{x,5}] + D[u[1][x,t],{x,7}];

diffFunctionListINPUT = {eq[1]};
numDependentVariablesINPUT = 1;
independentVariableListINPUT = {x};
nameINPUT = "Seventh order KdV Equation, parameterized";

parametersINPUT = {gg, ff, ee, dd, cc, bb, aa};
weightedParametersINPUT = {};

userWeightRulesINPUT = {};
rankRhoINPUT = Null;
explicitIndependentVariablesInDensitiesINPUT = Null;
formRhoINPUT = {}

(* d_7kdv.m *)
(* end of file *)
