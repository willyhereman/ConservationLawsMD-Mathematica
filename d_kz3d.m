(* d_kz3d.m *)
(* Menu item 2-21 *)

(* Last Updated:  2 September, 2008, 13:38 by DP at CSM *)

(* (3+1)-dimensional KHOKHLOV-ZABOLOTSKAYA (KZ) EQUATION                *)

eq[1] = D[D[u[1][x,y,z,t],t] - u[1][x,y,z,t]*D[u[1][x,y,z,t],x],x] -
        D[u[1][x,y,z,t],{y,2}] - D[u[1][x,y,z,t],{z,2}];

(* NOTE:  To put the KZ equation in evolution form, z and t are         *)
(* interchanged. Then an auxillary dependent variable is be introduced, *)
(* forming a system. The evolution system for the KZ equation is        *)
(*
eq[1] = D[u[1][x,y,z,t],t] - u[2][x,y,z,t];

eq[2] = D[u[2][x,y,z,t],t] - D[u[1][x,y,z,t],x,z] +
        D[u[1][x,y,z,t],x]^2 + u[1][x,y,z,t]*D[u[1][x,y,z,t],{x,2}] +
        D[u[1][x,y,z,t],{y,2}];
*)
diffFunctionListINPUT = {eq[1]};
numDependentVariablesINPUT = 1;
independentVariableListINPUT = {x,y,z};
nameINPUT = "(3+1)-dimensional Khokhlov-Zabolotskaya Equation";

parametersINPUT = {};
weightedParametersINPUT = {};

userWeightRulesINPUT = {};
rankRhoINPUT = Null;
explicitIndependentVariablesInDensitiesINPUT = Null;

(* formRhoINPUT only works with the evolution system.  The conservation *)
(* laws being tested must be transformed to match the evolution system. *)
formRhoINPUT = {};

(* REFERENCES:                                                          *)
(* N. H. Ibragimov, ed., The CRC Handbook of Lie Group Analysis of      *)
(* Differential Equations, vol.1 (1994), ch. 13, p. 299                 *)

(* Paper: J. Sanders and J. P. Wang, Hodge decomposition and conserva-  *)
(* tion laws, Math. Comput. Simul., V. 44 (1997), pp. 483-493.          *)

(* N. O. Sharomet, Symmetries, invariant solutions and                  *)
(* conservation laws of the nonlinear acoustics equation,               *)
(* Acta Appl. Math., V. 15 (1989), pp. 83-120.                          *)

(* d_3dkz.m *)
(* end of file *)
