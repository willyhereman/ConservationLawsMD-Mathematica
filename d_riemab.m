(* d_riemab.m *)
(* Menu item 1-31 *)

(* Last Updated:  11 June, 2007, 7:13 by DP at CSM *)

(***  RIEMAN SYSTEM  ***)

eq[1] = D[u[1][x,t],t] - u[2][x,t]*D[u[1][x,t],x];

eq[2] = D[u[2][x,t],t] - u[1][x,t]*D[u[2][x,t],x]

diffFunctionListINPUT = {eq[1], eq[2]};
numDependentVariablesINPUT = 2;
independentVariableListINPUT = {x};
nameINPUT = "Rieman System";

parametersINPUT = {};
weightedParametersINPUT = {};

userWeightRulesINPUT = {};
rankRhoINPUT = Null;
explicitIndependentVariablesInDensitiesINPUT = Null;
formRhoINPUT = {};

(* The Rieman System admits rational densities.  The program can test   *)
(* these densities.                                                     *)
(* formRhoINPUT = 1/(u[1][x,t] - u[2][x,t]); *)

(*
formRhoINPUT = (u[1][x,t] + u[2][x,t])/(u[1][x,t] - u[2][x,t]);
*)

(*
formRhoINPUT = (u[1][x,t]^2 + u[2][x,t]^2 +
                u[1][x,t]*u[2][x,t])/(u[1][x,t] - u[2][x,t])^2;
*)

(*
formRhoINPUT = (u[1][x,t]^3 + u[1][x,t]^2*u[2][x,t] +
                u[1][x,t]*u[2][x,t]^2+u[2][x,t]^3)/(u[1][x,t] - u[2][x,t])^3;
                *)

(*
formRhoINPUT = (u[1][x,t]^4 + u[1][x,t]^3*u[2][x,t] + u[1][x,t]^2*u[2][x,t]^2 +
                u[1][x,t]*u[2][x,t]^3 + u[2][x,t]^4)/(u[1][x,t] - u[2][x,t])^4;
*)   
                
(*
formRhoINPUT = (c[1]*u[1][x,t])/(u[1][x,t] - u[2][x,t]) +
(c[2]*u[2][x,t])/(u[1][x,t] - u[2][x,t]);
*)
                
(* d_riemab.m *)
(* end of file *)
