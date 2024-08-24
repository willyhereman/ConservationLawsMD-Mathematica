(* d_burg3d.m *)
(* Menu item 2-28 *)

(* Last Updated:  15 June, 2009, 11:25 by DP at CSM *)

(*** (3+1)-dimensional BURGERS EQUATION  ***)


eq[1] = D[D[u[1][x,y,z,t],t] + u[1][x,y,z,t]*D[u[1][x,y,z,t],x] -
        D[u[1][x,y,z,t],{x,2}],x] + D[u[1][x,y,z,t],{y,2}] +
        D[u[1][x,y,z,t],{z,2}];

(* NOTE:  To put the Burgers equation in evolution form, y and t must   *)
(* be interchanged. Then an auxillary dependent variable is be          *)
(* introduced, forming a system. The evolution system for the Burgers   *)
(* equation is                                                          *)
(*
eq[1] = D[u[1][x,y,z,t],t] - u[2][x,y,z,t];

eq[2] =  D[u[2][x,y,z,t],t] + D[D[u[1][x,y,z,t],z] +
         u[1][x,y,z,t]*D[u[1][x,y,z,t],x] - D[u[1][x,y,z,t],{x,2}],x] +
         D[u[1][x,y,z,t], {y,2}];
*)

diffFunctionListINPUT = {eq[1]};
numDependentVariablesINPUT = 1;
independentVariableListINPUT = {x,y,z};
nameINPUT = "the (3+1) Burgers equation";

parametersINPUT = {};
weightedParametersINPUT = {};

userWeightRulesINPUT = {};
rankRhoINPUT = Null;
explicitIndependentVariablesInDensitiesINPUT = Null;

(* formRhoINPUT only works with the evolution system.  The conservation *)
(* laws being tested must be transformed to match the evolution system. *)
formRhoINPUT = {};

(* REFERENCE:                                                           *)
(* Z. Lu and H. Zhang, Soliton-like and period form solutions for       *)
(* higher dimensional nonlinear evolution equations,                    *)
(* Chaos, Solitons, and Fractals, V. 17 (2003), pp. 669-673.            *)

(* d_burg3d.m *)
(* end of file *)
