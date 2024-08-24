(* d_zk3d.m *)
(* Menu item 2-2 *)

(* Last Updated: 8 July, 2008, 17:26 by DP at CSM *)

(***  GENERALIZED (3+1) ZAKHAROV-KUZNETSOV EQUATION  ***)

n = 1;

eq[1] = D[u[1][x,y,z,t],t] + alpha*u[1][x,y,z,t]^n*D[u[1][x,y,z,t],x] +
        beta*(D[u[1][x,y,z,t],{x,3}] + D[D[u[1][x,y,z,t],{y,2}],x] +
        D[D[u[1][x,y,z,t],{z,2}],x]);

diffFunctionListINPUT = {eq[1]};
numDependentVariablesINPUT = 1;
independentVariableListINPUT = {x,y,z};
nameINPUT = "Generalized (3+1)-dimensional Zakharov-Kuznetsov Equation";
noteINPUT = "The exponent in the term "<>
    ToString[Superscript[u, "n"], FormatType -> TraditionalForm]<>
    ToString[Subscript[u, x], FormatType -> TraditionalForm]<>
    " is set to n = "<>ToString[n]<>".";

parametersINPUT = {lalpha,beta};
weightedParametersINPUT = {};

userWeightRulesINPUT = {};
rankRhoINPUT = Null;
explicitIndependentVariablesInDensitiesINPUT = Null;
formRhoINPUT = {};

(*
Clear[n];
formRhoINPUT = {u[1][x,y,z,t] + u[1][x,y,z,t]^2 + u[1][x,y,z,t]^(n+2) -
               (beta/alpha)*(Derivative[0,0,1,0][u[1]][x,y,z,t]^2 +
               Derivative[0,1,0,0][u[1]][x,y,z,t]^2 +
               Derivative[1,0,0,0][u[1]][x,y,z,t]^2)};
*)

(* REFERENCES:                                                          *)
(* V. E. Zakharov and E. A. Kuznetsov, Three-dimensional solitons,      *)
(* Sov. Phys. JETP, V. 2 (1974), pp. 285-286.                           *)

(* A. M. Wazwaz, Exact solutions with solitons and perodic structures   *)
(* for the Zakharov-Kuznetsov (ZK) equation and its modified form,      *)
(* Commun. Nonlinear Sci. Numer. Simul., V. 10 (2005), pp. 597-606.     *)

(* d_zk3d.m *)
(* end of file *)
