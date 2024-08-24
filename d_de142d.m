(* d_de142d.m *)
(* Menu item 2-16 *)

(* Last Updated:  13 March, 2009, 9:48 by DP at CSM *)
(* Note: THIS IS A NON-POLYNOMIAL CASE !!! *)

(***  SHALLOW WATER MAGNETOHYDRODYNAMICS EQUATIONS  ***)
(* Equations (14a)-(14c) 2D system for fluid dynamics by Paul Dellar    *)

(* Note: THIS IS A NON-POLYNOMIAL CASE !!!                              *)

(* Assignment of the variables:                                         *)
(* u[1] = u     (first component velocity),                             *)
(* u[2] = v     (second component velocity),                            *)
(* u[3] = theta                                                         *)
(* u[4] = h                                                             *)

eq[1] = D[u[1][x,y,t],t] + u[1][x,y,t]*D[u[1][x,y,t],x] +
        u[2][x,y,t]*D[u[1][x,y,t],y] - 2*omega*u[2][x,y,t] +
        g*D[u[4][x,y,t],x] +
        (D[u[3][x,y,t],x]/u[4][x,y,t]^3)*(D[u[3][x,y,t],{y,2}]*u[4][x,y,t] -
        D[u[4][x,y,t],y]*D[u[3][x,y,t],y]) +
        (D[u[3][x,y,t],y]/u[4][x,y,t]^3)*(-D[D[u[3][x,y,t],x],y]*u[4][x,y,t] +
        D[u[4][x,y,t],x]*D[u[3][x,y,t],y]);

eq[2] = D[u[2][x,y,t],t]+ u[1][x,y,t]*D[u[2][x,y,t],x] +
        u[2][x,y,t]*D[u[2][x,y,t],y] + 2*omega*u[1][x,y,t] +
        g*D[u[4][x,y,t],y] +
        (D[u[3][x,y,t],x]/u[4][x,y,t]^3)*(-D[D[u[3][x,y,t],x],y]*u[4][x,y,t] +
        D[u[4][x,y,t],y]*D[u[3][x,y,t],x]) +
        (D[u[3][x,y,t],y]/u[4][x,y,t]^3)*(D[u[3][x,y,t],{x,2}]*u[4][x,y,t] -
        D[u[4][x,y,t],x]*D[u[3][x,y,t],x]);

eq[3] = D[u[3][x,y,t],t] + u[1][x,y,t]*D[u[3][x,y,t],x] +
        u[2][x,y,t]*D[u[3][x,y,t],y];

eq[4] = D[u[4][x,y,t],t] + u[1][x,y,t]*D[u[4][x,y,t],x] +
        u[2][x,y,t]*D[u[4][x,y,t],y] + u[4][x,y,t]*D[u[1][x,y,t],x] +
        u[4][x,y,t]*D[u[2][x,y,t],y];

diffFunctionListINPUT = Table[eq[i],{i,1,4}];
numDependentVariablesINPUT = 4;
independentVariableListINPUT = {x,y};
nameINPUT = "Fluid dynamics: Magnetohydrodynamics Equations";

parametersINPUT = {};
weightedParametersINPUT = {omega};

userWeightRulesINPUT = {};
rankRhoINPUT = Null;
explicitIndependentVariablesInDensitiesINPUT = Null;
formRhoINPUT = {};

(* The Hamiltionian forms a density, although this one is not in        *)
(* polynomial form !!! It must be tested separately.                    *)
(*
formRhoINPUT = {(1/2)*( u[4][x,y,t]*(u[1][x,y,t]^2+u[2][x,y,t]^2)+
  (1/u[4][x,y,t])*(D[u[3][x,y,t],x]^2+D[u[3][x,y,t],y]^2)+
  g*u[4][x,y,t]^2 )};
*)

(* General forms for densities *)
(*
formRhoINPUT = u[3][x,y,t]^n*u[4][x,y,t];
*)
(*
formRhoINPUT = (omega - D[u[1][x,y,t],y] + D[u[2][x,y,t],x])*u[3][x,y,t]^n;
*)

(* Further general forms for densities *)
(*
formRhoINPUT = ff[u[3][x,y,t]]*u[4][x,y,t] +
               (omega - D[u[1][x,y,t],y] + D[u[2][x,y,t],x])*gg[u[3][x,y,t]];
*)

(* REFERENCE:                                                           *)
(* P. Dellar, Common Hamiltonian structure in the shallow water         *)
(* equations with horizontal temperature gradients and magnetic fields. *)
(* Physics of Fluids, V. 15 (2003), pp. 292-297.                        *)

(* d_de142d.m *)
(* end of file *)
