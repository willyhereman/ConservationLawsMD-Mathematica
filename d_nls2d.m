(* d_nls2d.m *)
(* Menu item 2-29 *)

(* Last Updated:  15 June, 2009, 11:56, by DP at CSM *)

(***  (2+1)-dimensional NONLINEAR SCHRODINGER EQUATIONS ***)
(*     with real and imaginary parts                                    *)

eq[1] = D[u[1][x,y,t],t] + D[u[2][x,y,t],{x,2}] + D[u[2][x,y,t],{y,2}] +
        aa*u[2][x,y,t]*(u[1][x,y,t]^2 + u[2][x,y,t]^2);

eq[2] = -D[u[2][x,y,t],t] + D[u[1][x,y,t],{x,2}] + D[u[1][x,y,t],{y,2}] +
        aa*u[1][x,y,t]*(u[1][x,y,t]^2 + u[2][x,y,t]^2);

diffFunctionListINPUT = {eq[1], eq[2]};
numDependentVariablesINPUT = 2;
independentVariableListINPUT = {x,y};
nameINPUT = "the (2+1)-dimensional Nonlinear Schrodinger Equations with"<>
            " real and imaginary parts";

parametersINPUT = {aa};
weightedParametersINPUT = {};

userWeightRulesINPUT = {};
rankRhoINPUT = Null;
explicitIndependentVariablesInDensitiesINPUT = Null;
formRhoINPUT = {};

(* REFERENCE:                                                           *)
(* A. Polyanin and V. Zaitsev, Handbook of Nonlinear Partial            *)
(* Differential Equations, Chapman and Hall (2004), p. 186.             *)

(* d_nls2d.m *)
(* end of file *)
