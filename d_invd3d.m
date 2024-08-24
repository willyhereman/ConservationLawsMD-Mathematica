(* d_invd3d.m *)
(* Menu item 2-36 *)

(* Last Updated:   15 June, 2009, 17:00 by DP at CSM *)

(*** (3+1)-dimensional EQUATIONS FOR THE ISENTROPIC MOTION OF A  ***)
(*** INVISCID GAS                                                ***)

(* Assignments for variables are                                        *)
(* u[1] = u                                                             *)
(* u[2] = v                                                             *)
(* u[3] = w                                                             *)
(* u[4] = theta                                                         *)

eq[1] = D[u[1][x,y,z,t],t] + u[1][x,y,z,t]*D[u[1][x,y,z,t],x] +
        u[2][x,y,z,t]*D[u[1][x,y,z,t],y] + u[3][x,y,z,t]*D[u[1][x,y,z,t],z] +
        D[u[4][x,y,z,t],x];

eq[2] = D[u[2][x,y,z,t],t] + u[1][x,y,z,t]*D[u[2][x,y,z,t],x] +
        u[2][x,y,z,t]*D[u[2][x,y,z,t],y] + u[3][x,y,z,t]*D[u[2][x,y,z,t],z] +
        D[u[4][x,y,z,t],y];

eq[3] = D[u[3][x,y,z,t],t] + u[1][x,y,z,t]*D[u[3][x,y,z,t],x] +
        u[2][x,y,z,t]*D[u[3][x,y,z,t],y] + u[3][x,y,z,t]*D[u[3][x,y,z,t],z] +
        D[u[4][x,y,z,t],z];

eq[4] = D[u[4][x,y,z,t],t] + u[1][x,y,z,t]*D[u[4][x,y,z,t],x] +
        u[2][x,y,z,t]*D[u[4][x,y,z,t],y] + u[3][x,y,z,t]*D[u[4][x,y,z,t],z] +
        kappa*u[4][x,y,z,t]*(D[u[1][x,y,z,t],x] + D[u[2][x,y,z,t],y] +
        D[u[3][x,y,z,t],z]);

diffFunctionListINPUT = Table[eq[i], {i,1,4}];
numDependentVariablesINPUT = 4;
independentVariableListINPUT = {x,y,z};
nameINPUT = "(3+1)-dimensional isentropic motion of a inviscid gas";

parametersINPUT = {kappa};
weightedParametersINPUT = {};

userWeightRulesINPUT = {};
rankRhoINPUT = Null;
explicitIndependentVariablesInDensitiesINPUT = Null;
formRhoINPUT = {};

(* REFERENCE:                                                           *)
(* S. Meleshko, Methods for Constructing Exact Solutions for Partial    *)
(* Differential Equations,(2005), p. 73                                 *)

(* d_invd3d.m *)
(* end of file *)
