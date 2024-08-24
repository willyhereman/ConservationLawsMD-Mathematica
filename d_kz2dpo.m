(* d_kz2d.m *)
(* Menu item 2-19 *)

(* Last Updated:  15 June, 2009, 8:55 by DP at CSM *)

(*** (2+1)-dimensional POTENTIAL KHOKHLOV-ZABOLOTSKAYA (PKZ) EQUATION  ***)

eq[1] = D[u[1][x,y,t],x,t] - D[u[1][x,y,t],x]*D[u[1][x,y,t],{x,2}] -
        D[u[1][x,y,t],{y,2}];

(* NOTE:  To put the PKZ equation in evolution form, y and t must be    *)
(* interchanged. Then an auxillary dependent variable is be introduced, *)
(* forming a system. The evolution system for the PKZ equation is       *)
(*
eq[1] = D[u[1][x,y,t],t] - u[2][x,y,t];

eq[2] = D[u[2][x,y,t],t] - D[u[1][x,y,t],x,y] +
        D[u[1][x,y,t],x]*D[u[1][x,y,t],{x,2}];
*)

diffFunctionListINPUT = {eq[1]};
numDependentVariablesINPUT = 1;
independentVariableListINPUT = {x,y};
nameINPUT = "the (2+1)-dimensional Khokhlov-Zabolotskaya Equation";

parametersINPUT = {};
weightedParametersINPUT = {};

userWeightRulesINPUT = {};
rankRhoINPUT = Null;
explicitIndependentVariablesInDensitiesINPUT = Null;

(* formRhoINPUT only works with the evolution system.  The conservation *)
(* laws being tested must be transformed to match the evolution system. *)
formRhoINPUT = {};

(* REFERENCE:                                                           *)
(* V. Rosenhaus, Boundary conditions and conserved densities for the    *)
(* potential Zabolotskaya-Khokhlov equation,                            *)
(* J. Nonlin. Math. Phys., V. 14 (2006), pp. 255-270.                   *)

(* d_kz2d.m *)
(* end of file *)
