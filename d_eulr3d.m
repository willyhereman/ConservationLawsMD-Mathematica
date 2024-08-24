(* d_eulr3d.m *)
(* Menu item 2-26 *)

(* Last Updated:  15 June, 2009, 11:22 by DP at CSM *)

(***  (3+1)-dimensional EULER EQUATIONS  ***)
(*    u_t + u u_x + v u_y + w u_z = -p_x                                *)
(*    v_t + u v_x + v v_y + w v_z = -p_y                                *)
(*    w_t + u w_x + v w_y + w w_z = -p_z                                *)
(*                u_x + v_y + w_z = 0                                   *)

(* u[1][x,y,z,t], u[2][x,y,z,t], and u[3][x,y,z,t] are components of    *)
(* the velocity vector and u[4][x,y,z,t] is the pressure function.      *)

(* NOTE:  The program will not convert the given Euler equations into   *)
(* evolution form.  To put the Euler equations in evolution form,       *)
(* x and t are interchanged.  The evolution system for the Euler        *)
(* equations is                                                         *)

eq[1] = D[u[1][x,y,z,t],t] + D[u[2][x,y,z,t],y] + D[u[3][x,y,z,t],z];

eq[2] = u[1][x,y,z,t]*D[u[2][x,y,z,t],t] + D[u[2][x,y,z,t],x] +
        u[2][x,y,z,t]*D[u[2][x,y,z,t],y] + u[3][x,y,z,t]*D[u[2][x,y,z,t],z] +
        D[u[4][x,y,z,t],y];

eq[3] = u[1][x,y,z,t]*D[u[3][x,y,z,t],t] + D[u[3][x,y,z,t],x] +
        u[2][x,y,z,t]*D[u[3][x,y,z,t],y] + u[3][x,y,z,t]*D[u[3][x,y,z,t],z] +
        D[u[4][x,y,z,t],z];

eq[4] = D[u[4][x,y,z,t],t] + D[u[1][x,y,z,t],x] -
        u[1][x,y,z,t]*D[u[2][x,y,z,t],y] - u[1][x,y,z,t]*D[u[3][x,y,z,t],z] +
        u[2][x,y,z,t]*D[u[1][x,y,z,t],y] +  u[3][x,y,z,t]*D[u[1][x,y,z,t],z];


diffFunctionListINPUT = Table[eq[i], {i,1,4}];
numDependentVariablesINPUT = 4;
independentVariableListINPUT = {x,y,z};
nameINPUT = "(3+1)-dimensional Euler Equations";
noteINPUT = "To create the conditions for evolution form, the variables"<>
   " x and t have been switched with each other."

parametersINPUT = {};
weightedParametersINPUT = {};

userWeightRulesINPUT = {};
rankRhoINPUT = Null;
explicitIndependentVariablesInDensitiesINPUT = Null;
formRhoINPUT = {};

(* REFERENCE:                                                           *)
(* P. Olver, Applications of Lie Groups to Differential Equations,      *)
(* Springer (1993), p. 127                                              *)

(* d_eulr3d.m *)
(* end of file *)
