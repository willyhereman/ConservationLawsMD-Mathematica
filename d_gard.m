(* d_gard.m *)
(* Menu item 1-32 *)

(* Last Updated:  18 June, 2008, 15:16 by DP at CSM *)

(***  GARDNER EQUATION  ***)
(* Often referred to as a combined KdV and modified KdV equation        *)
(* See Konopelchenkp and Dubrovsky, Phys. Lett. 102A, April, 1984.      *)


eq[1] = D[u[1][x,t],t] + gamma*u[1][x,t]*D[u[1][x,t],x] +
        alpha* u[1][x,t]^2*D[u[1][x,t],x] +
        beta*D[u[1][x,t],{x,3}];

diffFunctionListINPUT = {eq[1]};
numDependentVariablesINPUT = 1;
independentVariableListINPUT = {x};
nameINPUT = "Gardner Equation";

parametersINPUT = {alpha, beta};
weightedParametersINPUT = {gamma};

userWeightRulesINPUT = {};
rankRhoINPUT = Null;
explicitIndependentVariablesInDensitiesINPUT = Null;
formRhoINPUT = {};

(* REFERENCE:                                                           *) 
(* Z. Fu, S. Liu, and S. Li, New kinds of solutions to Gardner          *)
(* Equation, Chaos, Fractals and Solitons, V. 20 (2004), pp. 301-309.   *)

(* d_gard.m *)
(* end of file *)
