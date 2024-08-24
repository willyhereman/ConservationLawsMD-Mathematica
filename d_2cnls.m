(* d_2cnls.m *)
(* Menu item 1-29 *)

(* Last Updated:  14 May, 2007, 13:01 by DP at CSM *)

(***  TWO-COMPENENT NONLINEAR SCHRODINGER EQUATION  ***)

eq[1] = D[u[1][x,t],t] + D[u[3][x,t],{x,2}] +
        2*Sum[u[i][x,t]^2, {i,1,4}]*u[3][x,t];

eq[2] = D[u[2][x,t],t] + D[u[4][x,t],{x,2}] +
        2*Sum[u[i][x,t]^2, {i,1,4}]*u[4][x,t];

eq[3] = D[u[3][x,t],t] - D[u[1][x,t],{x,2}] -
        2*Sum[u[i][x,t]^2, {i,1,4}]*u[1][x,t];

eq[4] = D[u[4][x,t],t] - D[u[2][x,t],{x,2}] -
        2*Sum[u[i][x,t]^2, {i,1,4}]*u[2][x,t];

diffFunctionListINPUT = Table[eq[i], {i,1,4}];
numDependentVariablesINPUT = 4;
independentVariableListINPUT = {x};
nameINPUT = "2-Compenent Nonlinear Schrodinger Equation";

parametersINPUT = {};
weightedParametersINPUT = {};

userWeightRulesINPUT = {};
rankRhoINPUT = Null;
explicitIndependentVariablesInDensitiesINPUT = Null;
formRhoINPUT = {};

(* d_2cnls.m *)
(* end of file *)
