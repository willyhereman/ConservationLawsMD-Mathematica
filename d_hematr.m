(* d_hematr.m *)
(* Menu item 1-38 *)

(* Last Updated:  13 May, 2008, 10:16 by DP at CSM *)

(* This equation is a special case that occurs in nonlinear problems    *)
(* involving heat and mass transfer, combustion theory and flows in     *)
(* porous media.                                                        *)

(*eq[1] = D[u[1][x,t],t] - aa*D[u[1][x,t],{x,2}]/u[1][x,t]^2 +
          aa*D[u[1][x,t],x]^2/u[1][x,t]^3;*)

eq[1] = u[1][x,t]^3*D[u[1][x,t],t] - aa*D[u[1][x,t],{x,2}]*u[1][x,t] +
        aa*D[u[1][x,t],x]^2;

diffFunctionListINPUT = {eq[1]};
numDependentVariablesINPUT = 1;
independentVariableListINPUT = {x};
nameINPUT = "(1+1) heat/mass transfer equation";
noteINPUT = Null;

parametersINPUT = {aa};
weightedParametersINPUT = {};

userWeightRulesINPUT = {};
rankRhoINPUT = Null;
explicitIndependentVariablesInDensitiesINPUT = Null;
formRhoINPUT = {};

(* Equation 1.1.10.3, p. 26 Polyanin and Zaitsev, 2004, Handbook of     *)
(*     Nonlinear PDEs                                                   *)

(* d_hematr.m *)
(* end of file *)
