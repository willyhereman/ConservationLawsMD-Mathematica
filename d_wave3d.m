(* d_wave3d.m *)
(* Menu item 2-40 *)

(* Last Updated:   15 June, 2009, 18:46 by DP at CSM *)

(***  (3+1)-dimensional WAVE EQUATION  ***)

eq[1] = D[u[1][x,y,z,t],{t,2}] - D[u[1][x,y,z,t],{x,2}] -
        D[u[1][x,y,z,t],{y,2}] - D[u[1][x,y,z,t],{z,2}];

(* NOTE:  To put the wave equation in evolution form, an auxillary      *)
(* dependent variable must be introduced, forming a system. The         *)
(* evolution system for the wave equation is:                           *)
(*
eq[1] = D[u[1][x,y,z,t],t] - u[2][x,y,z,t];

eq[2] = D[u[2][x,y,z,t],t] - D[u[1][x,y,z,t],{x,2}] -
        D[u[1][x,y,z,t],{y,2}] - D[u[1][x,y,z,t],{z,2}];
*)

diffFunctionListINPUT = {eq[1]};
numDependentVariablesINPUT = 1;
independentVariableListINPUT = {x,y,z};
nameINPUT = "the (3+1)-dimensional Wave Equation";

parametersINPUT = {};
weightedParametersINPUT = {};

userWeightRulesINPUT = {};
rankRhoINPUT = Null;
explicitIndependentVariablesInDensitiesINPUT = Null;

(* formRhoINPUT only works with the evolution system.  The conservation *)
(* laws being tested must be transformed to match the evolution system. *)
formrho[x,y,z,t] = {};

(* REFERENCE:                                                           *)
(* J. Butcher, J. Carminati, K.T. Vu, A Comparative Study of the        *)
(* Computer Algebra Packages which Determine the Lie Point Symmetries   *)
(* of Differential Equations, (2003).                                   *)

(* d_wave3d.m *)
(* end of file *)
