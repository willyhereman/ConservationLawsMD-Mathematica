(* d_trag2d.m *)
(* Menu item 2-32 *)

(* Last Updated:  15 June, 2009, 12:30 by DP at CSM *)

(***  (2+1)-dimensional NONSTATIONARY TRANSON GAS FLOW (NTGF) EQUATION  ***)

eq[1] = 2*D[u[1][x,y,t],x,t] + D[u[1][x,y,t],x]*D[u[1][x,y,t],{x,2}] -
        D[u[1][x,y,t],{y,2}];

(* NOTE:  To put the NTGF equation in evolution form, y and t must be   *)
(* interchanged. Then an auxillary dependent variable is be introduced, *)
(* forming a system. The evolution system for the NTGF equation is      *)
(*
eq[1] = D[u[1][x,y,t],t] - u[2][x,y,t];

eq[2] = D[u[2][x,y,t],t] - 2*D[u[1][x,y,t],x,y] -
        D[u[1][x,y,t], x]*D[u[1][x,y,t], {x,2}];
*)

diffFunctionListINPUT = {eq[1]};
numDependentVariablesINPUT = 1;
independentVariableListINPUT = {x,y};
nameINPUT = "Modeling (2+1)-dimensional Nonstationary Transon Gas Flow";
(* noteINPUT = "To create evolution equations with respect to the t-variable, "<>
            "the variables y and t have been interchanged in the equations "<>
            "given to the program." *)

parametersINPUT = {};
weightedParametersINPUT = {};

userWeightRulesINPUT = {};
rankRhoINPUT = Null;
explicitIndependentVariablesInDensitiesINPUT = Null;

(* formRhoINPUT only works with the evolution system.  The conservation *)
(* laws being tested must be transformed to match the evolution system. *)
formRhoINPUT = {};


(* REFERENCE:                                                           *)
(* N. H. Ibragimov, ed., The CRC Handbook of Lie Group Analysis of      *)
(* Differential Equations, vol.1 (1994), ch. 13, p. 296                 *)

(* d_trag2d.m *)
(* end of file *)
