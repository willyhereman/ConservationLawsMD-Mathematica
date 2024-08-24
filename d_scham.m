(* d_scham.m *)
(* Menu item 1-12 *)

(* Last Updated:  14 May, 2007, 13:25 by DP at CSM *)

(***  SCHAMEL EQUATION  ***)

(* Schamel equation-original *)
(* 16*D[u[x,t],t] + 30*Sqrt[u[x,t]]*D[u[x,t],x] + D[u[x,t],{x,3}] == 0; *)

(* Schamel equation-after transformation u^(1/2) -> u *)

eq[1] = 16*u[1][x,t]*D[u[1][x,t],t] + 30*u[1][x,t]^2*D[u[1][x,t],x] +
        3*D[u[1][x,t],x]*D[u[1][x,t],{x,2}] + u[1][x,t]*D[u[1][x,t],{x,3}];

diffFunctionListINPUT = {eq[1]};
numDependentVariablesINPUT = 1;
independentVariableListINPUT = {x};
nameINPUT = "Schamel Equation";
noteINPUT = "The Schamel Equation has been transformed by replacing u^(1/2)"<>
    " in the original equation with u in the equation used by the program."

parametersINPUT = {};
weightedParametersINPUT = {};

userWeightRulesINPUT = {};
rankRhoINPUT = Null;
explicitIndependentVariablesInDensitiesINPUT = Null;
formRhoINPUT = {};

formrho[x,t] = {};

(* Taken from: Conservation laws and solitary wave solutions for         *)
(*             generalized Schamel equations.  Verheest and Hereman,     *)
(*             Physica Scripta, (50), 1995                               *)

(* d_scham.m *)
