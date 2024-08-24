(* d_kz3d.m *)
(* Menu item 2-22 *)

(* Last Updated:  15 June, 2009, 10:00 by DP at CSM *)

(***  (3+1)-dimensional POTENTIAL KHOKHLOV-ZABOLOTSKAYA (PKZ) EQUATION  ***)

eq[1] = D[u[1][x,y,z,t],x,t] - D[u[1][x,y,z,t],x]*D[u[1][x,y,z,t],{x,2}] -
        D[u[1][x,y,z,t],{y,2}] - D[u[1][x,y,z,t],{z,2}];

(* NOTE:  To put the PKZ equation in evolution form, z and t are        *)
(* interchanged. Then an auxillary dependent variable is be introduced, *)
(* forming a system. The evolution system for the PKZ equation is       *)
(*
eq[1] = D[u[1][x,y,z,t],t] - u[2][x,y,z,t];

eq[2] = D[u[2][x,y,z,t],t] - D[u[1][x,y,z,t],x,z] +
        D[u[1][x,y,z,t],x]*D[u[1][x,y,z,t],{x,2}] +
        D[u[1][x,y,z,t],{y,2}];
*)

diffFunctionListINPUT = {eq[1]};
numDependentVariablesINPUT = 1;
independentVariableListINPUT = {x,y,z};
nameINPUT = "the (3+1)-dimensional potential Khokhlov-Zabolotskaya Equation";

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

(* d_3dkz.m *)
(* end of file *)
