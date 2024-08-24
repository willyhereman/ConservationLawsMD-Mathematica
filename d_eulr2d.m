(* d_eulr2d.m *)
(* Menu item 2-25 *)

(* Last Updated:  15 June, 2009, 11:02 by DP at CSM *)

(***  (2+1)-dimensional EULER EQUATIONS  ***)
(*    u_t + u u_x + u v_y = -p_x                                        *)
(*    v_t + u v_x + v v_y = -p_y                                        *)
(*              u_x + v_y = 0                                           *)

(* u[1][x,y,t] and u[2][x,y,t] are components of the velocity vector    *)
(* u[3][x,y,t] is the pressure function                                 *)

(* NOTE:  The program will not convert the given Euler equations into   *)
(* evolution form.  To put the Euler equations in evolution form,       *)
(* x and t are interchanged.  The evolution system for the Euler        *)
(* equations is                                                         *)

eq[1] = D[u[1][x,y,t],t] + D[u[2][x,y,t],y];

eq[2] = u[1][x,y,t]*D[u[2][x,y,t],t] + D[u[2][x,y,t],x] +
        u[2][x,y,t]*D[u[2][x,y,t],y] + D[u[3][x,y,t],y];

eq[3] = D[u[3][x,y,t],t] + D[u[1][x,y,t],x] -
        u[1][x,y,t]*D[u[2][x,y,t],y] +
        u[2][x,y,t]*D[u[1][x,y,t],y];

diffFunctionListINPUT = Table[eq[i], {i,1,3}];
numDependentVariablesINPUT = 3;
independentVariableListINPUT = {x,y};
nameINPUT = "(2+1) Euler Equation";
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

(* d_eulr2d.m *)
(* end of file *)
