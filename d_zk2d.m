(* d_zk2d.m *)
(* Menu item 2-1 *)

(* Last Updated: 6 August, 2008, 16:12 by DP at CSM *)

(***  GENERALIZED (2+1)-DIMENSIONAL ZAKHAROV-KUZNETSOV EQUATION  ***)

n = 1;

eq[1] = D[u[1][x,y,t],t] +
        alpha*u[1][x,y,t]^n*D[u[1][x,y,t],x] +
        beta*( D[u[1][x,y,t],{x,3}] + D[D[u[1][x,y,t],{y,2}],x] );

diffFunctionListINPUT = {eq[1]};
numDependentVariablesINPUT = 1;
independentVariableListINPUT = {x,y};
nameINPUT = "Generalized (2+1) Zakharov-Kuznetsov Equation";
noteINPUT = "The exponent in the term "<>
    ToString[Superscript[u, "n"], FormatType -> TraditionalForm]<>
    ToString[Subscript[u, x], FormatType -> TraditionalForm]<>
    " is set to n = "<>ToString[n]<>".";

parametersINPUT = {alpha,beta};
weightedParametersINPUT = {};

userWeightRulesINPUT = {};
rankRhoINPUT = Null;
explicitIndependentVariablesInDensitiesINPUT = Null;
formRhoINPUT = {};

(*
Clear[n];
formRhoINPUT = {u[1][x,y,t] + u[1][x,y,t]^2 + u[1][x,y,t]^(n+2) -
               (beta/alpha)*(D[u[1][x,y,t],x]^2 + D[u[1][x,y,t],y]^2)};
*)

(* REFERENCES:                                                          *)
(* V. E. Zakharov and E. A. Kuznetsov, Three-dimensional solitons,      *)
(* Sov. Phys. JETP, V. 2 (1974), pp. 285-286.                           *)

(* A. M. Wazwaz, Exact solutions with solitons and perodic structures   *)
(* for the Zakharov-Kuznetsov (ZK) equation and its modified form,      *)
(* Commun. Nonlinear Sci. Numer. Simul., V. 10 (2005), pp. 597-606.     *)

(* d_zk2d.m *)
(* end of file *)
