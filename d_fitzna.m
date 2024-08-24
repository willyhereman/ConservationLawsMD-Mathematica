(* d_fitzna.m *)
(* Menu 1-35 *)

(* Last Updated:  18 June, 2008, 16:46 by DP at CSM *)

(***  FITZHUGH-NAGUMO EQUATION  ***)

(* The Fitzhugh-Nagumo Equation is given in this form:                  *)
(* D[u[1][x,t],t] = D[u[1][x,t],{x,2}] -                                *)
(*            u[1][x,t]*(1 - u[1][x,t])*(alpha - u[1][x,t]);            *)

(* To be able to add the necessary weighted parameters, the equation    *)
(* is rewritten with bb and cc as weighted parameters.                  *)

eq[1] = D[u[1][x,t],t] - D[u[1][x,t],{x,2}] + bb*u[1][x,t] -
        cc*(1 + alpha)*u[1][x,t]^2 + u[1][x,t]^3;

diffFunctionListINPUT = {eq[1]};
numDependentVariablesINPUT = 1;
independentVariableListINPUT = {x};
nameINPUT = "Fitzhugh-Nagumo Equation";

parametersINPUT = {alpha};
weightedParametersINPUT = {bb,cc};

userWeightRulesINPUT = {};
rankRhoINPUT = Null;
explicitIndependentVariablesInDensitiesINPUT = Null;
formRhoINPUT = {};

(* From Polyanin and Zaitsev, Handbook for Nonlinear PDEs, 2004, p. 4 *)

(* d_fitzna.m *)
(* end of file *)
