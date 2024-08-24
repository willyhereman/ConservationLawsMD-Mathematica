(* d_poly2d.m *)
(* Menu item 2-35 *)

(* Last Updated:   15 June, 2009, 12:45 by DP at CSM *)

(*** (2+1)-dimensional EQUATIONS FOR THE ISENTROPIC MOTION OF A  ***)
(*** INVISCID GAS                                                ***)

(* Assignments for variables are                                        *)
(* u[1] = u                                                             *)
(* u[2] = v                                                             *)
(* u[3] = theta                                                         *)

eq[1] = D[u[1][x,y,t],t] + u[1][x,y,t]*D[u[1][x,y,t],x] +
        u[2][x,y,t]*D[u[1][x,y,t],y] + D[u[3][x,y,t],x];

eq[2] = D[u[2][x,y,t],t] + u[1][x,y,t]*D[u[2][x,y,t],x] +
        u[2][x,y,t]*D[u[2][x,y,t],y] + D[u[3][x,y,t],y];

eq[3] = D[u[3][x,y,t],t] + u[1][x,y,t]*D[u[3][x,y,t],x] +
        u[2][x,y,t]*D[u[3][x,y,t],y] +
        kappa*u[3][x,y,t]*(D[u[1][x,y,t],x] + D[u[2][x,y,t],y]);

diffFunctionListINPUT = Table[eq[i], {i,1,3}];
numDependentVariablesINPUT = 3;
independentVariableListINPUT = {x,y};
nameINPUT = "(2+1)-dimensional isentropic motion of a inviscid gas";

parametersINPUT = {kappa};
weightedParametersINPUT = {};

userWeightRulesINPUT = {};
rankRhoINPUT = Null;
explicitIndependentVariablesInDensitiesINPUT = Null;
formRhoINPUT = {};

(* REFERENCE:                                                           *)
(* S. Meleshko, Methods for Constructing Exact Solutions for Partial    *)
(* Differential Equations,(2005), p. 73                                 *)

(* d_poly2d.m *)
(* end of file *)
