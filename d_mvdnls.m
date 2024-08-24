(* d_mvdnls.m *)
(* Menu item 1-21 *)

(* Last Updated:  14 May, 2007, 13:20 by DP at CSM *)

(***  MODIFIED VECTOR DERIVATIVE NONLINEAR SCHRODINGER SYSTEM (MVDNLS)  ***)
(***  Note the extra term with `beta'                                   ***)

eq[1] = D[u[1][x,t],t] + D[u[1][x,t]*(u[1][x,t]^2 + u[2][x,t]^2) +
        beta*u[1][x,t] - D[u[2][x,t],x],x];

eq[2] = D[u[2][x,t],t] + D[u[2][x,t]*(u[1][x,t]^2 + u[2][x,t]^2) +
        D[u[1][x,t],x],x];

diffFunctionListINPUT = {eq[1], eq[2]};
numDependentVariablesINPUT = 2;
independentVariableListINPUT = {x};
nameINPUT = "Modified Vector Derivative Nonlinear Schrodinger System (MVDNLS)";

parametersINPUT = {};
weightedParametersINPUT = {beta};

userWeightRulesINPUT = {};
rankRhoINPUT = Null;
explicitIndependentVariablesInDensitiesINPUT = Null;
formRhoINPUT = {};

(* d_mvdnls.m *)
(* end of file *)
