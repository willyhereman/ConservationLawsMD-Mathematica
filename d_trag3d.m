(* d_trag3d.m *)
(* Menu item 2-33 *)

(* Last Updated:  15 June, 2009, 12:36 by DP at CSM *)

(***  (3+1)-dimensional NONSTATIONARY TRANSONIC GAS FLOW (NTGF) EQUATION  ***)


eq[1] = 2*D[u[1][x,y,z,t],x,t] + D[u[1][x,y,z,t],x]*D[u[1][x,y,z,t],{x,2}] -
        D[u[1][x,y,z,t],{y,2}] - D[u[1][x,y,z,t],{z,2}];

(* NOTE:  To put the NTGF equation in evolution form, z and t are       *)
(* interchanged. Then an auxillary dependent variable is be introduced, *)
(* forming a system. The evolution system for the NTGF equation is      *)
(*
eq[1] = D[u[1][x,y,z,t],t] - u[2][x,y,z,t] ;

eq[2] = D[u[2][x,y,z,t],t] - 2*D[u[1][x,y,z,t],x,z] -
        D[u[1][x,y,z,t],x]*D[u[1][x,y,z,t],{x,2}] +
        D[u[1][x,y,z,t],{y,2}];
*)

diffFunctionListINPUT = {eq[1]};
numDependentVariablesINPUT = 1;
independentVariableListINPUT = {x,y,z};
nameINPUT = "(3+1)-dimensional Nonstationary Transonic Gas Flow Equation";
(* noteINPUT = "To create evolution equations with respect to the t-variable, "<> 
             "the variables y and t have been switched with each other in "<>   
             "the equations given to the program."                              
*)

parametersINPUT = {};
weightedParametersINPUT = {};

userWeightRulesINPUT = {};
rankRhoINPUT = Null;
explicitIndependentVariablesInDensitiesINPUT = Null;

(* formRhoINPUT only works with the evolution system.  The conservation *)
(* laws being tested must be transformed to match the evolution system. *)
formRhoINPUT = {};

(* Generalized densities with an arbitrary function of independent variables *)
(*
formRhoINPUT = {(x*D[f[y],y] + D[f[y],{y,2}]*z^2)*u[2][x,y,z,t] +
               f[y]*D[u[1][x,y,z,t],x]*u[2][x,y,z,t]};
*)
(*
formRhoINPUT = {f[y]*(u[2][x,y,z,t]^2 + D[u[1][x,y,z,t],z]^2 +
               u[1][x,y,z,t]*D[u[1][x,y,z,t],x,y] +
               u[1][x,y,z,t]*D[u[1][x,y,z,t],x]*D[u[1][x,y,z,t],{x,2}]) +
               D[f[y],y]*t*(u[1][x,y,z,t]*D[u[2][x,y,z,t],x] +
               u[2][x,y,z,t]*D[u[1][x,y,z,t],x]) + (D[f[y],{y,2}]*x +
               D[f[y],{y,3}]*z^2)*(u[1][x,y,z,t] + t*u[2][x,y,z,t])};
*)
(*
formRhoINPUT = {f[y]*D[u[1][x,y,z,t],z]u[2][x,y,z,t] +
               D[f[y],y]*z*u[1][x,y,z,t]*D[u[2][x,y,z,t],x] +
               (x*z*D[f[y],{y,2}] + z^3*D[f[y],{y,3}])*(u[2][x,y,z,t])};
*)

(* REFERENCE:                                                           *)
(* N. H. Ibragimov, ed., The CRC Handbook of Lie Group Analysis of      *)
(* Differential Equations, vol.1 (1994), ch. 13, p. 298                 *)

(* d_trag3d.m *)
(* end of file *)
