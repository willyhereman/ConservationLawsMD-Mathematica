(* d_klgo3d.m *)
(* Menu item 2-31 *)

(* Last Updated:  15 June, 2009, 11:05 by DP at CSM *)

(***  (3+1)-dimensional KLEIN-GORDON (KG) EQUATION  ***)

eq[1] = D[u[1][x,y,z,t],{t,2}] - D[u[1][x,y,z,t],{x,2}] -
        D[u[1][x,y,z,t],{y,2}] - D[u[1][x,y,z,t],{z,2}] +
        mm^2*u[1][x,y,z,t];

(* NOTE:  To put the KG equation in evolution form, an auxillary        *)
(* dependent variable must be introduced, forming a system. The         *)
(* evolution system for the KG equation is:                             *)
(*
eq[1] = D[u[1][x,y,z,t],t] - u[2][x,y,z,t];

eq[2] = D[u[2][x,y,z,t],t] - D[u[1][x,y,z,t],{x,2}] -
        D[u[1][x,y,z,t],{y,2}] - D[u[1][x,y,z,t],{z,2}] +
        mm^2*u[1][x,y,z,t];
*)

diffFunctionListINPUT = {eq[1]};
numDependentVariablesINPUT = 1;
independentVariableListINPUT = {x,y,z};
nameINPUT = "(3+1)-dimensional Klein-Gordon Equation";

parametersINPUT = {};
weightedParametersINPUT = {mm};

userWeightRulesINPUT = {};
rankRhoINPUT = Null;
explicitIndependentVariablesInDensitiesINPUT = Null;

(* formRhoINPUT only works with the evolution system.  The conservation *)
(* laws being tested must be transformed to match the evolution system. *)
formRhoINPUT = {};

(* REFERENCE:                                                           *)
(* P. Menzala, On inverse scattering for the Klein-Gordon equation with *)
(* small potentials, Funkcialaj Elvacioj, V. 20 (1977), p. 61-70.       *)

(* d_klgo3d.m *)
(* end of file *)
