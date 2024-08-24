(* d_navi3d.m *)
(* Menu item 2-9 *)

(* Last Updated:  13 November, 2008, 9:14 by DP at CSM *)

(***  NAVIER'S EQUATION  ***)

(* Here rho (constant density of material) = aa, and the Lame           *)
(* constants, lambda = ll and mu = mm                                   *)

(* To run higher ranks, ll + mm is replaced with bb.                    *)

eq[1] = aa*D[u[1][x,y,z,t],{t,2}] - bb*(D[u[1][x,y,z,t],{x,2}] +
        D[u[2][x,y,z,t],x,y] + D[u[3][x,y,z,t],x,z]) -
        mm*(D[u[1][x,y,z,t],{x,2}] + D[u[1][x,y,z,t],{y,2}] +
        D[u[1][x,y,z,t],{z,2}]);

eq[2] = aa*D[u[2][x,y,z,t],{t,2}] - bb*(D[u[1][x,y,z,t],x,y] +
        D[u[2][x,y,z,t],{y,2}] + D[u[3][x,y,z,t],y,z]) -
        mm*(D[u[2][x,y,z,t],{x,2}] + D[u[2][x,y,z,t],{y,2}] +
        D[u[2][x,y,z,t],{z,2}]);

eq[3] = aa*D[u[3][x,y,z,t],{t,2}] - bb*(D[u[1][x,y,z,t],x,z] +
        D[u[2][x,y,z,t],y,z] + D[u[3][x,y,z,t],{z,2}]) -
        mm*(D[u[3][x,y,z,t],{x,2}] + D[u[3][x,y,z,t],{y,2}] +
        D[u[3][x,y,z,t],{z,2}]);

(* NOTE:  To put Navier's equation in evolution form, three auxillary   *)
(* dependent variables must be introduced, forming a system. The        *)
(* evolution system for Navier's equation is:                           *)
(*
eq[1] = u[4][x,y,z,t] - D[u[1][x,y,z,t],t];

eq[2] = u[5][x,y,z,t] - D[u[2][x,y,z,t],t];

eq[3] = u[6][x,y,z,t] - D[u[3][x,y,z,t],t];

eq[4] = aa*D[u[4][x,y,z,t],t] - bb*(D[u[1][x,y,z,t],{x,2}] +
        D[u[2][x,y,z,t],x,y] + D[u[3][x,y,z,t],x,z]) -
        mm*(D[u[1][x,y,z,t],{x,2}] + D[u[1][x,y,z,t],{y,2}] +
        D[u[1][x,y,z,t],{z,2}]);

eq[5] = aa*D[u[5][x,y,z,t],t] - bb*(D[u[1][x,y,z,t],x,y] +
        D[u[2][x,y,z,t],{y,2}] + D[u[3][x,y,z,t],y,z]) -
        mm*(D[u[2][x,y,z,t],{x,2}] + D[u[2][x,y,z,t],{y,2}] +
        D[u[2][x,y,z,t],{z,2}]);

eq[6] = aa*D[u[6][x,y,z,t],t] - bb*(D[u[1][x,y,z,t],x,z] +
        D[u[2][x,y,z,t],y,z] + D[u[3][x,y,z,t],{z,2}]) -
        mm*(D[u[3][x,y,z,t],{x,2}] + D[u[3][x,y,z,t],{y,2}] +
        D[u[3][x,y,z,t],{z,2}]);
*)

diffFunctionListINPUT = Table[eq[i], {i, 1, 3}]
numDependentVariablesINPUT = 3;
independentVariableListINPUT = {x, y, z};
nameINPUT = "Navier Equation";
noteINPUT = "To increase efficiency, the parameter bb has replaced the sum"<>
    " of the Lame constants, lambda + mu, in the equation."

parametersINPUT = {bb, aa, mm};
weightedParametersINPUT = {};

userWeightRulesINPUT = {};
rankRhoINPUT = Null;
explicitIndependentVariablesInDensitiesINPUT = Null;
formRhoINPUT = {};

(* REFERENCE:                                                           *) 
(* J. Billingham and A. C. King, Wave Motion,                           *) 
(* Cambridge University Press, (2000), p. 132.                          *) 

(* d_navi3d.m *)
(* end of file *)
