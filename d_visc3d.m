(* d_3dvisc.m *)
(* Menu item 2-34 *)

(* Last Updated:  15 June, 2009, 12:43 by DP at CSM *)

(***  (3+1)-dimensional EQUATION FOR VISCOUS TRANSONIC FLOW (VTF)  ***)

eq[1] = 2*D[u[1][x,y,z,t],x,t] + D[u[1][x,y,z,t],x]*D[u[1][x,y,z,t],{x,2}] -
        mu*D[u[1][x,y,z,t],{x,3}] - D[u[1][x,y,z,t],{y,2}] -
        D[u[1][x,y,z,t],{z,2}];

(* NOTE:  To put the VTF equation in evolution form, z and t are        *)
(* interchanged. Then an auxillary dependent variable is be introduced, *)
(* forming a system. The evolution system for the VTF equation is       *)
(*
eq[1] = D[u[2][x,y,z,t],t] - u[1][x,y,z,t];

eq[2] = D[u[1][x,y,z,t],t] - 2*D[u[2][x,y,z,t],x,z] -
        D[u[2][x,y,z,t],x]*D[u[2][x,y,z,t],{x,2}] +
        D[u[2][x,y,z,t],{y,2}] + mu*D[u[2][x,y,z,t],{x,3}];
*)

diffFunctionListINPUT = {eq[1]};
numDependentVariablesINPUT = 1;
independentVariableListINPUT = {x,y,z};
nameINPUT = "(3+1)-dimensional viscous transonic flow";

parametersINPUT = {};
weightedParametersINPUT = {mu};

userWeightRulesINPUT = {};
rankRhoINPUT = Null;
explicitIndependentVariablesInDensitiesINPUT = Null;

(* formRhoINPUT only works with the evolution system.  The conservation *)
(* laws being tested must be transformed to match the evolution system. *)
formRhoINPUT = {};

(* S. Igonin, Conservation Laws for Multidimensional Systems and     *)
(* Related Linear Algebra Problems,                                  *)
(* J. Phys. A: Math. Gen., V. 35 (2002), pp. 10607-10617.            *)

(* d_3dvisc.m *)
(* end of file *)
