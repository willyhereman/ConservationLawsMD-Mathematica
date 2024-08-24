(* d_ch2d.m *)
(* Menu item 2-24 *)

(* Last Updated: 17 August, 2008, 8:32 by DP at CSM *)

(***  (2+1)-dimensional CAMASSA-HOLM EQUATION  ***)

(* Note: Weighted parameters alpha and beta have been added to the      *)
(* equation.  No scaling symmetry exists without these parameters.      *)

eq[1] = D[alpha*D[u[1][x,y,t],t] + kappa D[u[1][x,y,t],x] -
        D[u[1][x,y,t],{x,2},t] + 3*beta*u[1][x,y,t]*D[u[1][x,y,t],x] -
        2*D[u[1][x,y,t],x]*D[u[1][x,y,t],{x,2}] -
        u[1][x,y,t]*D[u[1][x,y,t],{x,3}],x] + D[u[1][x,y,t],{y,2}];

(* NOTE:  To put the CH equation in evolution form, y and t must be     *)
(* interchanged. Then an auxillary dependent variable is be introduced, *)
(* forming a system. The evolution system for the CH equation is        *)
(*
eq[1] = D[u[1][x,y,t],t] - u[2][x,y,t];

eq[2] = D[u[2][x,y,t],t] + D[alpha*D[u[1][x,y,t],y] + kappa D[u[1][x,y,t],x] -
        D[u[1][x,y,t],{x,2},y] + 3*beta*u[1][x,y,t]*D[u[1][x,y,t],x] -
        2*D[u[1][x,y,t],x]*D[u[1][x,y,t],{x,2}] -
        u[1][x,y,t]*D[u[1][x,y,t],{x,3}],x];
*)

diffFunctionListINPUT = {eq[1]};
numDependentVariablesINPUT = 1;
independentVariableListINPUT = {x,y};
nameINPUT = "(2+1) Camassa-Holm Equation";

parametersINPUT = {};
weightedParametersINPUT = {alpha,beta,kappa};

userWeightRulesINPUT = {};
rankRhoINPUT = Null;
explicitIndependentVariablesInDensitiesINPUT = Null;

(* formRhoINPUT only works with the evolution system.  The conservation *)
(* laws being tested must be transformed to match the evolution system. *)
formRhoINPUT = {};

(* Forms for the generalized conservation laws.  The densities given    *)
(* match the evolution form for the CH equation.                        *)

(*
formRhoINPUT = ((1/alpha)*x*f[y] + D[f[y],y]*t^2)*(u[1][x,y,t]-t*u[2][x,y,t]) +
     ((1/alpha)*x*f[y] + D[f[y],y]*t^2)*u[2][x,y,t] + D[f[y],y]*t*u[1][x,y,t];
*)

(* REFERENCE:                                                           *)
(* P. G. Gordoa, A. Pickering, and M. Senthilevan, Evidence for the     *)
(* Nonintegrability of a Water Wave Equation in (2+1) dimensions,       *)
(* Zeitschrift fur Naturforschung, V. 59 (2004), pp. 640-644.           *)

(* d_ch2d.m *)
(* end of file *)
