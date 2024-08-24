(* d_htra2d.m *)
(* Menu item 2-38 *)

(* Last Updated:  26 October, 2007, 11:29 by DP at CSM *)

(***  (2+1)-dimensional EQUATION FOR HEAT TRANSFER WITH LINEAR  ***)
(***  TEMPERATURE-DEPENDENT THERMAL CONDUCTIVITY                ***)

eq[1] = D[u[1][x,y,t],t] -
        D[(aa*u[1][x,y,t] + bb)*D[u[1][x,y,t],x],x] -
        D[(aa*u[1][x,y,t] + bb)*D[u[1][x,y,t],y],y];

diffFunctionListINPUT = {eq[1]};
numDependentVariablesINPUT = 1;
independentVariableListINPUT = {x,y};
nameINPUT = "the (2+1)-dimensional equation for heat transfer with linear "<>
            "temperature dependent thermal conductivity";

parametersINPUT = {aa};
weightedParametersINPUT = {bb};
userWeightRulesINPUT = {};
rankRhoINPUT = Null;
explicitIndependentVariablesInDensitiesINPUT = Null;
formRhoINPUT = {};

(* REFERENCE:                                                           *)
(* A. Polyanin and V. Zaitsev, Handbook of Nonlinear Partial            *)
(* Differential Equations, Chapman and Hall (2004), p. 148.             *)

(* d_htra2d.m *)
(* end of file *)
