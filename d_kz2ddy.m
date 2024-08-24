(* d_kz2ddy.m *)
(* Menu item 2-20 *)

(* Last Updated:  15 June, 2009, 9:36 by DP at CSM *)

(*** y-differentiated (2+1)-dimensional KHOKHLOV-ZABOLOTSKAYA EQUATION  ***)

eq[1] = D[u[1][x,y,t],x,y,t] - 2*D[u[1][x,y,t],x]*D[u[1][x,y,t],x,y] -
        D[u[1][x,y,t],y]*D[u[1][x,y,t],{x,2}] -
        u[1][x,y,t]*D[u[1][x,y,t],{x,2},y] - D[u[1][x,y,t],{y,3}];

(* NOTE:  To put the y-differentiated KZ equation in evolution form,    *)
(* y and t must be interchanged, and two auxillary dependent variables  *)
(* must be introduced, forming a system. The evolution system for the   *)
(* y-differentiated KZ equation is                                      *)
(*
eq[1] = D[u[1][x,y,t],t] - u[2][x,y,t];

eq[2] = D[u[2][x,y,t],t] - u[3][x,y,t];

eq[3] = D[u[3][x,y,t],t] - D[u[2][x,y,t],x,y] +
        2*D[u[1][x,y,t],x]*D[u[2][x,y,t],x] +
        u[2][x,y,t]*D[u[1][x,y,t],{x,2}] +
        u[1][x,y,t]*D[u[2][x,y,t],{x,2}];
*)

diffFunctionListINPUT = {eq[1]};
numDependentVariablesINPUT = 1;
independentVariableListINPUT = {x,y};
nameINPUT = "the y-differentiated (2+1)-dimensional Khokhlov-Zabolotskaya"<>
            " Equation";

parametersINPUT = {};
weightedParametersINPUT = {};

userWeightRulesINPUT = {};
rankRhoINPUT = Null;
explicitIndependentVariablesInDensitiesINPUT = Null;

(* formRhoINPUT only works with the evolution system.  The conservation *)
(* laws being tested must be transformed to match the evolution system. *)
formRhoINPUT = {};

(* REFERENCE:                                                           *)
(* A. R. Chowdhury and M. Nasker, Towards the conservation laws         *)
(* and Lie symmetries of the Khokhlov-Zabolotkaya equation in three     *)
(* dimensions, J. Phys. A: Math. Gen., V. 19 (1986), pp. 1775-1781.     *)

(* d_kz2ddy.m *)
(* end of file *)
