(* d_kauk2d.m *)
(* Menu item 2-12 *)

(* Last Updated:  14 June, 2009, 18:56 by DP at CSM *)

(***  A (2+1)-dimensional version of the KAUP-KUPERSCHMIDT (KK) EQUATION  ***)

eq[1] = D[u[1][x,y,t],t] - (5*u[1][x,y,t]^2*D[u[1][x,y,t],x] +
        5*u[1][x,y,t]*D[u[1][x,y,t],{x,3}] +
        5*D[u[1][x,y,t],x]*D[u[1][x,y,t],{x,2}] + D[u[1][x,y,t],{x,5}] +
        5*D[u[2][x,y,t],{x,3}] + 5*u[1][x,y,t]*D[u[2][x,y,t],x] +
        5*D[u[1][x,y,t],x]*u[2][x,y,t] - 5*D[u[2][x,y,t],y]);

eq[2] = D[u[1][x,y,t],y] - D[u[2][x,y,t],x];

(* NOTE:  To put the KK equation in evolution form, y and t must        *)
(* interchanged.  The evolution system for the KK equation is           *)
(*
eq[1] = D[u[1][x,y,t],{x,5}] + 5*u[1][x,y,t]*D[u[1][x,y,t],{x,3}] +
        (25/2)*D[u[1][x,y,t],x]*D[u[1][x,y,t],{x,2}] +
        5*u[1][x,y,t]^2*D[u[1][x,y,t],x] + 5*D[u[2][x,y,t],{x,3}] -
        5*D[u[2][x,y,t],t] + 5*u[1][x,y,t]*D[u[2][x,y,t],x] +
        5*D[u[1][x,y,t],x]*u[2][x,y,t] - D[u[1][x,y,t],y];

eq[2] = -D[u[1][x,y,t],t] + D[u[2][x,y,t],x];
*)

diffFunctionListINPUT = {eq[1], eq[2]};
numDependentVariablesINPUT = 2;
independentVariableListINPUT = {x, y};
nameINPUT = "the (2+1)-dimensional Kaup-Kupershmidt Equation";
(* noteINPUT = "To create evolution equations with respect to the t-variable, "<>
            "the variables y and t have been switched with each other in the "<>
            "equations given to the program." *)

parametersINPUT = {};
weightedParametersINPUT = {};

userWeightRulesINPUT = {};
rankRhoINPUT = Null;
explicitIndependentVariablesInDensitiesINPUT = Null;

(* formRhoINPUT only works with the evolution system.  The conservation *)
(* laws being tested must be transformed to match the evolution system. *)
formRhoINPUT = {};

(* REFERENCE:                                                           *) 
(* B. G. Konopelchenko and V. G. Dubrovsky, Some new integrable         *)
(* nonlinear evolution equations in 2+1 dimensions                      *)
(* Phys. Lett., V. 102A, (1984), pp.15-17.                              *)

(* d_kauk2d.m *)
(* end of file *)
