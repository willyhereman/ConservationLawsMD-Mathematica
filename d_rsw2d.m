(* d_rsw2d.m *)
(* Menu item 2-14 *)

(* Last Updated:  15 June, 2009, 8:25 by DP at CSM *)

(***  (2+1)-dimensional ROTATING SHALLOW WATER EQUATIONS  ***)

(* Assignments for variables are                                        *)
(* u[1] = u                                                             *)
(* u[2] = v                                                             *)
(* u[3] = h                                                             *)
(* u[4] = h_B                                                           *)

(* There is only one parameter, the Coriolis parameter, designated ff.  *)
(* g is a constant representing acceleration due to gravity.            *)

eq[1] = D[u[1][x,y,t],t] + u[1][x,y,t]*D[u[1][x,y,t],x] +
        u[2][x,y,t]*D[u[1][x,y,t],y] - ff*u[2][x,y,t] +
        g*D[u[3][x,y,t],x];

eq[2] = D[u[2][x,y,t],t] + u[1][x,y,t]*D[u[2][x,y,t],x] +
        u[2][x,y,t]*D[u[2][x,y,t],y] + ff*u[1][x,y,t] +
        g*D[u[3][x,y,t],y];

eq[3] = D[u[3][x,y,t],t] +
        ((u[3][x,y,t]-u[4][x,y,t])*D[u[1][x,y,t],x] +
        u[1][x,y,t]*D[(u[3][x,y,t] - u[4][x,y,t]),x] +
        (u[3][x,y,t]-u[4][x,y,t])*D[u[2][x,y,t],y] +
        u[2][x,y,t]*D[(u[3][x,y,t]-u[4][x,y,t]),y]);

eq[4] = D[u[4][x,y,t],t]


diffFunctionListINPUT = Table[eq[i],{i, 1, 4}];
numDependentVariablesINPUT = 4;
independentVariableListINPUT = {x,y};
nameINPUT = "the (2+1)-dimensional Rotating Shallow Water Equations";

parametersINPUT = {};
weightedParametersINPUT = {ff};

userWeightRulesINPUT = {};
rankRhoINPUT = Null;
explicitIndependentVariablesInDensitiesINPUT = Null;
formRhoINPUT = {};

(* REFERENCES:                                                          *)
(* D. Gurarie, Long-Range Dynamics of a Shallow Water Triad:            *)
(* Renormalization, Modularization, and Cyclogenesis                    *)
(* J. of Atmospheric Sciences, V. 60, (2003), pp. 693-710.              *)

(* J. Pedlosky, Geophysical Fluid dynamics (pp. 59-63).                 *)

(* d_rsw2d.m *)
(* end of file *)
