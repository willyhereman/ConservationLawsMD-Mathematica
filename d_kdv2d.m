(* d_kdv2d.m *)
(* Menu item 2-13 *)

(* Last Updated:  14 June, 2009, 19:35 by DP at CSM *)

(***  (2+1) INTEGRABLE COUPLING OF THE KDV EQUATION  ***)


eq[1] = D[u[1][x,y,t],t] - 6*u[1][x,y,t]*D[u[1][x,y,t],x] -
               D[u[1][x,y,t],{x,3}]

eq[2] = D[u[2][x,y,t],t] - (D[u[2][x,y,t],{x,3}] +
        3*D[u[1][x,y,t],{x,2},y] + 6*u[1][x,y,t]*D[u[2][x,y,t],x] +
        6*u[2][x,y,t]*D[u[1][x,y,t],x] + 6*u[1][x,y,t]*D[u[1][x,y,t],y]);

diffFunctionListINPUT = {eq[1], eq[2]};
numDependentVariablesINPUT = 2;
independentVariableListINPUT = {x,y};
nameINPUT = "the (2+1)-dimensional Integrable Coupling of the KdV Equation";

parametersINPUT = {};
weightedParametersINPUT = {};

userWeightRulesINPUT = {};
rankRhoINPUT = Null;
explicitIndependentVariablesInDensitiesINPUT = Null;
formRhoINPUT = {};

(* REFERENCE:                                                           *) 
(* Z. Yan, The (2+1)-dimensional integrable coupling of KdV equation:   *)
(* Auto-Backlund transformation and new non-traveling wave profiles,    *)
(* Phys. Lett. A, V. 345 (2005), pp. 362-377                            *)

(* d_kdv2d.m *)
(* end of file *)
