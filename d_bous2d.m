(* d_bous2d.m *)
(* Menu 2-27 *)

(* Last Updated:  15 June, 2009, 11:40 by DP at CSM *)

(***  (2+1)-dimensional BOUSSINESQ EQUATION  **)
(* for nonlinear heat conduction theory                                 *)

eq[1] = D[u[1][x,y,t],t] - D[u[1][x,y,t]*D[u[1][x,y,t],x],x] -
        D[u[1][x,y,t]*D[u[1][x,y,t],y],y];

diffFunctionListINPUT = {eq[1]};
numDependentVariablesINPUT = 1;
independentVariableListINPUT = {x,y};
nameINPUT = "(2+1)-dimensional Boussinesq Equation";

parametersINPUT = {};
weightedParametersINPUT = {};

userWeightRulesINPUT = {};
rankRhoINPUT = Null;
explicitIndependentVariablesInDensitiesINPUT = Null;
formRhoINPUT = {};

(* REFERENCE:                                                           *)
(* A. Polyanin and V. Zaitsev, Handbook of Nonlinear Partial            *)
(* Differential Equations, Chapman and Hall, (2004), p. 142.            *)

(* d_bous2d.m *)
(* end of file *)
