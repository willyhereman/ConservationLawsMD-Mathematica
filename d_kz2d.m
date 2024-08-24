(* d_kz2d.m *)
(* Menu item 2-18 *)

(* Last Updated:  1 September, 2008, 18:14 by DP at CSM *)

(***  (2+1)-dimensional KHOKHLOV-ZABOLOTSKAYA (ZK) EQUATION  ***)

eq[1] = D[D[u[1][x,y,t],t] - u[1][x,y,t]*D[u[1][x,y,t],x],x] -
        D[u[1][x,y,t],{y,2}];
        
(* NOTE:  To put the KZ equation in evolution form, y and t must be     *)
(* interchanged. Then an auxillary dependent variable is be introduced, *)
(* forming a system. The evolution system for the KZ equation is        *)
(*
eq[1] = D[u[1][x,y,t],t] -  u[2][x,y,t];

eq[2] = D[u[2][x,y,t],t] - D[u[1][x,y,t],x,y] +
        D[u[1][x,y,t],x]^2 + u[1][x,y,t]*D[u[1][x,y,t],{x,2}];
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

(* Generalized densities with f[y] replacing y^n.                       *)
(*
formRhoINPUT = (t*x*ff[y] + t^3*D[ff[y],y])*u[2][x,y,t] +
               (ff[y]*x + t^2*D[ff[y],y])*u[1][x,y,t];
*)
(*
Clear[gg];
formRhoINPUT = (x*gg[y] + t^2*D[gg[y],y])*u[2][x,y,t] +
               D[gg[y],y]*t*u[1][x,y,t];
*)

(* REFERENCES:                                                          *)
(* N. H. Ibragimov, ed., The CRC Handbook of Lie Group Analysis of      *)
(* Differential Equations, vol.1 (1994), ch. 13, p. 299                 *)

(* N. O. Sharomet, Symmetries, invariant solutions and                  *)
(* conservation laws of the nonlinear acoustics equation,               *)
(* Acta Appl. Math., V. 15 (1989), pp. 83-120.                          *)

(* d_kz2d.m *)
(* end of file *)
