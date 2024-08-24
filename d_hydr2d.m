(* d_hydr2d.m *)
(* Menu item 2-37 *)

(* Last Updated:   15 June, 2009, 17:25 by DP at CSM *)

(***  (2+1)-dimensional EQUATION FOR AN UNSTEADY HYDRODYNAMIC  ***)
(***  BOUNDARY LAYER (UHBL) ON A FLAT PLANE                    ***)

eq[1] = D[u[1][x,y,t],y,t] + D[u[1][x,y,t],y]*D[u[1][x,y,t],x,y] -
        D[u[1][x,y,t],x]*D[u[1][x,y,t],{y,2}] - nu*D[u[1][x,y,t],{y,3}];

(* NOTE:  To put the UHBL equation in evolution form, y and t must be   *)
(* interchanged. Then an auxillary dependent variable is be introduced, *)
(* forming a system. The evolution system for the UHBL equation is      *)
(*
eq[1] = D[u[1][x,y,t],t] - u[2][x,y,t];

eq[2] = D[u[2][x,y,t],t] - u[3][x,y,t];

eq[3] = D[u[3][x,y,t],t] - (1/nu)*(D[u[2][x,y,t],y] +
               u[2][x,y,t]*D[u[2][x,y,t],x] -
               D[u[1][x,y,t],x]*u[3][x,y,t]);
*)

diffFunctionListINPUT = {eq[1]};
numDependentVariablesINPUT = 1;
independentVariableListINPUT = {x,y};
nameINPUT = "the (2+1)-dimensional equation for an unsteady hydrodynamic"<>
            " boundary layer on a flat plane";
parametersINPUT = {nu};
weightedParametersINPUT = {};
userWeightRulesINPUT = {};
rankRhoINPUT = Null;
explicitIndependentVariablesInDensitiesINPUT = Null;

(* formRhoINPUT only works with the evolution system.  The conservation *)
(* laws being tested must be transformed to match the evolution system. *)
formRhoINPUT = {};

(* REFERENCE:                                                           *)
(* A. Polyanin and V. Zaitsev, Handbook of Nonlinear Partial            *)
(* Differential Equations, Chapman and Hall (2004), p. 553.             *)

(* d_hydr2d.m *)
(* end of file *)
