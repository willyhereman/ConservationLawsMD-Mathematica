(* d_vkar2d.m *)
(* Menu item 2-39 *)

(* Last Updated:   15 June, 2009, 17:35 by DP at CSM *)

(***  (2+1)-dimensional VON-KARMEN EQUATIONS  ***)

(* Assignment of the variables:                                         *)
(*  u[1] = f                                                            *)
(*  u[2] = w                                                            *)
(* NOTE:  Weighted parameters, {aa, bb, cc}, have been added to the     *)
(* second equation to give the system a scaling symmetry.               *)

eq[1] = D[u[1][x,y,t],{x,4}] + D[u[1][x,y,t],{y,4}] +
        2*D[u[1][x,y,t],{x,2},{y,2}] - D[u[2][x,y,t],x,y]^2 +
        D[u[2][x,y,t],{x,2}]*D[u[2][x,y,t],{y,2}];

eq[2] = aa*D[u[2][x,y,t],{x,4}] + bb*D[u[2][x,y,t],{y,4}] +
        2*cc*D[u[2][x,y,t],{x,2},{y,2}] + D[u[2][x,y,t],{t,2}] -
        D[u[1][x,y,t],{y,2}]*D[u[2][x,y,t],{x,2}] -
        D[u[1][x,y,t],{x,2}]*D[u[2][x,y,t],{y,2}] +
        2*D[u[1][x,y,t],x,y]*D[u[2][x,y,t],x,y];

(* NOTE:  To put the VK equations in evolution form, y and t must be    *)
(* interchanged. Then auxillary dependent variables are be introduced,  *)
(* forming a system. The evolution system for the VK equation is        *)
(*
eq[1] = D[u[1][x,y,t],t] - u[3][x,y,t] ;

eq[2] = D[u[3][x,y,t],t] -  u[4][x,y,t] ;

eq[3] = D[u[4][x,y,t],t] - u[5][x,y,t] ;

eq[4] = D[u[2][x,y,t],t] - u[6][x,y,t] ;

eq[5] = D[u[6][x,y,t],t] - u[7][x,y,t] ;

eq[6] = D[u[7][x,y,t],t] - u[8][x,y,t] ;

eq[7] = D[u[5][x,y,t],t] + D[u[1][x,y,t],{x,4}] +
               2*D[u[4][x,y,t],{x,2}] - D[u[6][x,y,t],x]^2 +
               u[7][x,y,t]*D[u[2][x,y,t],{x,2}];

eq[8][x,y,t] = D[u[8][x,y,t],t] + D[u[2][x,y,t],{x,4}] +
               2*D[u[7][x,y,t],{x,2}]) + D[u[2][x,y,t],{y,2}] -
               D[u[1][x,y,t],{x,2}]*u[7][x,y,t] -
               u[4][x,y,t]*D[u[2][x,y,t],{x,2}] +
               2*D[u[3][x,y,t],x]*D[u[6][x,y,t],x];
*)

diffFunctionListINPUT = Table[eq[i], {i,1,2}];
numDependentVariablesINPUT = 2;
independentVariableListINPUT = {x,y};
nameINPUT = "the (2+1)-dimensional Von-Karmen Equations";

parametersINPUT = {};
weightedParametersINPUT = {aa,bb,cc};

userWeightRulesINPUT = {};
rankRhoINPUT = Null;
explicitIndependentVariablesInDensitiesINPUT = Null;

(* formRhoINPUT only works with the evolution system.  The conservation *)
(* laws being tested must be transformed to match the evolution system. *)
formRhoINPUT = {};

(* REFERENCE:                                                           *)
(* J. Butcher, J. Carminati, K.T. Vu, A Comparative Study of the        *)
(* Computer Algebra Packages which Determine the Lie Point Symmetries   *)
(* of Differential Equations, (2003).                                   *)

(*d_vkar2d.m *)
(* end of file *)
