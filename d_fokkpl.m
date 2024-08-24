(* d_fokkpl.m *)
(* Menu item 1-34  *)

(* Last Updated:  14 May, 2008, 12:30 by DP at CSM *)

(***  FOKKER-PLANK EQUATION  ***)

eq[1] = D[u[1][x,t],t] - D[u[1][x,t],{x,2}] -
        beta*(x*D[u[1][x,t],x] + u[1][x,t]);

diffFunctionListINPUT = {eq[1]};
numDependentVariablesINPUT = 1;
independentVariableListINPUT = {x};
nameINPUT = "(1+1) Fokker-Plank Equation";

parametersINPUT = {};
weightedParametersINPUT = {beta};

userWeightRulesINPUT = {};
rankRhoINPUT = Null;
explicitIndependentVariablesInDensitiesINPUT = Null;
formRhoINPUT = {};

(* Taken from P. Olver, Applications of Lie Groups to Differential      *)
(* Equations, 1993, p. 177                                              *)

(* d_fokkpl.m *)
(* end of file *)
