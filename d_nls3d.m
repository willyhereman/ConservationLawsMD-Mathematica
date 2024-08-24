(* d_nls3d.m *)
(* Menu item 2-30 *)

(* Last Updated:  15 June, 2009, 12:06 by DP at CSM *)

(*** (3+1)-dimensional NONLINEAR SCHRODINGER EQUATIONS  ***)
(*    with real and imaginary parts                                     *)

eq[1] = D[u[1][x,y,z,t],t] + D[u[2][x,y,z,t],{x,2}] +
        D[u[2][x,y,z,t],{y,2}] + D[u[2][x,y,z,t],{z,2}] +
        aa*u[2][x,y,z,t]*(u[1][x,y,z,t]^2 + u[2][x,y,z,t]^2);

eq[2] = -D[u[2][x,y,z,t],t] + D[u[1][x,y,z,t],{x,2}] +
        D[u[1][x,y,z,t],{y,2}] + D[u[1][x,y,z,t],{z,2}] +
        aa*u[1][x,y,z,t]*(u[1][x,y,z,t]^2 + u[2][x,y,z,t]^2);

diffFunctionListINPUT = {eq[1], eq[2]};
numDependentVariablesINPUT = 2;
independentVariableListINPUT = {x,y,z};
nameINPUT = "the (3+1)-dimensional Nonlinear Schrodinger Equations"<>
            " with real and imaginary parts";

parametersINPUT = {aa};
weightedParametersINPUT = {};

userWeightRulesINPUT = {};
rankRhoINPUT = Null;
explicitIndependentVariablesInDensitiesINPUT = Null;
formRhoINPUT = {};

(* REFERENCE:                                                           *)
(* A. Polyanin and V. Zaitsev, Handbook of Nonlinear Partial            *)
(* Differential Equations, Chapman and Hall (2004), p. 189.             *)

(* d_nls3d.m *)
(* end of file *)
